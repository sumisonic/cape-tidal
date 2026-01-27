;;; cape-tidal.el --- TidalCycles completion backend -*- lexical-binding: t -*-

;; Copyright (C) 2024 sumisonic

;; Author: sumisonic
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, languages, tools
;; URL: https://github.com/sumisonic/cape-tidal

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A completion-at-point (capf) backend for TidalCycles.
;; It provides completion for TidalCycles functions and values using GHCi's
;; :complete command, and shows type information as annotations.
;;
;; It works with Corfu, Cape, or the standard completion-at-point.  It is a
;; plain capf and does not require Cape at runtime; Cape is only needed for the
;; cape-capf-super example below.
;;
;; Usage:
;;   (require 'cape-tidal)
;;   (add-hook 'tidal-mode-hook
;;             (lambda ()
;;               (add-hook 'completion-at-point-functions #'cape-tidal nil t)))
;;
;; With cape-capf-super:
;;   (add-hook 'tidal-mode-hook
;;             (lambda ()
;;               (add-hook 'completion-at-point-functions
;;                         (cape-capf-super
;;                          #'cape-tidal
;;                          #'haskell-completions-sync-repl-completion-at-point
;;                          #'cape-dabbrev)
;;                         nil t)))

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; The only runtime dependency is tidal.el.  Do not require it (loading must
;; not
;; be forced); only declare it at compile time (actual calls are guarded with
;; boundp / fboundp).
(defvar tidal-buffer)
;; Corfu is optional; we only touch it to redraw the popup when a type arrives.
(defvar corfu-mode)
(defvar corfu--base)
(defvar corfu--candidates)
(declare-function corfu--candidates-popup "corfu" (pos))
(declare-function tidal-send-string "tidal" (s))

;;; Customization variables

(defgroup cape-tidal nil
  "TidalCycles completion using Cape."
  :group 'completion
  :prefix "cape-tidal-")

(defcustom cape-tidal-candidates-limit 100
  "Maximum number of candidates passed to GHCi's :complete command."
  :type 'integer
  :group 'cape-tidal)

(defcustom cape-tidal-timeout 5.0
  "Timeout in seconds for a GHCi response."
  :type 'number
  :group 'cape-tidal)

(defcustom cape-tidal-prompt-regexp "^tidal>"
  "Regexp that detects the GHCi prompt."
  :type 'regexp
  :group 'cape-tidal)

(defcustom cape-tidal-drain-timeout 1.0
  "Longest time, in seconds, to swallow a late response from a discarded
  request.
After a timeout or a cancellation the response is still on the wire, so output
is swallowed (drained) up to the next prompt boundary.  Past this many seconds,
stop swallowing and fall back to the raw comint output (fail open).  This is a
safety valve against a silent REPL, so do not make it too large."
  :type 'number
  :group 'cape-tidal)

(defcustom cape-tidal-interrupt-drain-timeout 0.3
  "Longest time, in seconds, to keep draining after the user's evaluation
  interrupts.
When the user evaluates a pattern during a completion request, completion loses
immediately, and only its own late response is swallowed, up to this many
seconds.  Past the deadline the accumulated output is returned to comint (never
hiding the user's evaluation output is the top priority)."
  :type 'number
  :group 'cape-tidal)

(defcustom cape-tidal-prefetch-limit 20
  "Per-completion budget of background `:type' requests.
Candidates are fetched in the order they are requested for display
\(`cape-tidal--annotation-function' emits a demand hint).  A larger value gives
more complete annotations but sends more background requests to GHCi, which is
more likely to compete with playing (pattern evaluation)."
  :type 'integer
  :group 'cape-tidal)

(defcustom cape-tidal-prefetch-delay 0.2
  "Quiet delay, in seconds, between each background `:type' request.
Leaving this gap between sends opens a window for a pattern evaluation to
interrupt.  It is also the retry interval when GHCi was too busy to send."
  :type 'number
  :group 'cape-tidal)

(defcustom cape-tidal-sync-timeout 2.0
  "Longest time, in seconds, a completion may block Emacs.
While `cape-tidal-timeout' is the deadline of the GHCi communication state
machine, this is the limit on how long the user is made to wait.  Past it,
return no candidates and let the next keystroke retry.  Giving up does not
corrupt internal state (a response that arrives late is discarded)."
  :type 'number
  :group 'cape-tidal)

;;; Internal variables (buffer-local)
;;
;; There is only ever one request in flight (single-flight).  State
;; transitions:
;;   nil (idle) --transmit--> active --boundary reached--> nil
;;   active --timeout/cancel--> draining --boundary or deadline--> nil
;; `draining' is the state where "the callback is dead but the response is
;; still
;; on the wire": output is swallowed (with a deadline) up to the next prompt
;; boundary.

(defvar-local cape-tidal--state nil
  "Request state.  One of nil (idle), `active', or `draining'.")

(defvar-local cape-tidal--output nil
  "Buffer that accumulates output from GHCi.")

(defvar-local cape-tidal--callback nil
  "Callback of the current request.  Non-nil only while `active'.")

(defvar-local cape-tidal--deadline nil
  "Absolute deadline of the current state (a `float-time' value).
Past it, `cape-tidal--preoutput-filter' lifts all suppression (fail open).
It is always set while `cape-tidal--state' is non-nil.")

(defvar-local cape-tidal--interrupted nil
  "Non-nil when the current drain is due to the user's evaluation interrupting.
On fail open in this case the accumulated output may contain the user's
evaluation result, so the accumulated part is also returned to comint.")

(defvar-local cape-tidal--pending nil
  "A waiting request, as a list (REQUEST CALLBACK ENQUEUE-TIME).
One slot only (latest wins).  For completion requests only; type prefetches are
not queued here.")

(defvar-local cape-tidal--foreign-pending 0
  "Number of foreign sends (the user's evaluations, etc.) with no response yet.
Incremented by the advice on `tidal-send-string' and by the comint input hook,
decremented by the number of prompts seen in idle passthrough output. 
cape-tidal
does not send until it reaches 0.")

(defvar-local cape-tidal--last-foreign-time nil
  "Time a foreign send was last noticed (a `float-time' value).
Used to recover when the counter gets stuck because a prompt was missed across
a
chunk boundary.")

(defvar-local cape-tidal--prefetch-generation 0
  "Generation token for the type prefetch.
Incremented when a new completion session starts and by
`cape-tidal--hard-reset';
timers of an old generation do nothing when they fire.  It is not changed by a
foreign send — during playing this is a `pause', not an `invalidate' (see
`cape-tidal--prefetch-step').")

(defvar-local cape-tidal--prefetch-queue nil
  "Work queue for the type prefetch: candidate names not yet sent `:type'.
Sent from the front.  Reordered by the demand hint from
`cape-tidal--annotation-function'.")

(defvar-local cape-tidal--prefetch-demand nil
  "FIFO of unfetched candidates whose display was requested (annotation
  called).
Drained before `cape-tidal--prefetch-queue'.  Appended at the tail to preserve
display order.")

(defvar-local cape-tidal--prefetch-budget 0
  "Remaining number of background `:type' sends allowed in this completion session.")

(defconst cape-tidal--idle-tail-max 64
  "Maximum number of characters kept in `cape-tidal--idle-tail'.
With a `cape-tidal-prompt-regexp' that matches a prompt longer than this, a
prompt split across a chunk boundary may be missed (the time-based forced
recovery eventually restores things).")

(defvar-local cape-tidal--idle-tail ""
  "Trailing fragment of idle output (after the last counted prompt).
comint hands over process output split at arbitrary positions, so the prompt
`tidal> ' can arrive split into `tidal' and `> '.  Matching per chunk would
fail
to count it, the foreign-send counter would stall, and both the completion
pending and the prefetch would stop.  Kept so it can be concatenated with the
next chunk and matched.")

(defvar-local cape-tidal--idle-tail-bol t
  "Non-nil when `cape-tidal--idle-tail' starts at a line beginning (or right
  after a counted prompt).
A marker so that `^' does not match by mistake when a fragment truncated at the
limit starts in the middle of a line.")

(defvar-local cape-tidal--in-foreign-block nil
  "Non-nil when a foreign send is inside a `:{' ... `:}' block.
tidal.el sends a multi-line evaluation as three `tidal-send-string' calls
(`:{',
the body, `:}'), but GHCi returns a single prompt (`prompt-cont' is empty).
Counting all three would stall the counter at 2 and stop both the prefetch and
the completion pending until the 2-second forced recovery.  Count the whole
block as one foreign send.")

(defvar cape-tidal--refresh-timer nil
  "Pending completion-popup redraw (a debounce so several become one).")

(defvar cape-tidal--refresh-retries 0
  "Number of times a redraw was skipped because input was pending.
Retried up to a limit.")

(defvar cape-tidal--debug nil
  "When non-nil, log each popup-redraw decision with `message' (for bug reports).")

(defvar-local cape-tidal--last-foreign-done nil
  "Time a foreign send's response last finished (a `float-time').
For `cape-tidal-prefetch-delay' seconds after this, the prefetch does not send
\(quiet delay).")

(defvar-local cape-tidal--prefetch-timer nil
  "One-shot timer that has the next prefetch step scheduled.
nil if none is scheduled.")

(defvar-local cape-tidal--process nil
  "The tidal process that was set up.
Used to detect a GHCi restart (a process swap in the same buffer).")

(defvar cape-tidal--own-send nil
  "Dynamically bound to non-nil while cape-tidal itself is sending.
The advice on `tidal-send-string' reads it to tell its own sends apart from
foreign sends (evaluation of patterns).")

(defvar-local cape-tidal--request-id 0
  "Serial number of a request.
When a timer fires it compares this value with its own ID, so that an old timer
that escaped cancellation cannot corrupt a later request's state.")

(defvar-local cape-tidal--timeout-timer nil
  "Timer for the timeout.")

(defvar-local cape-tidal--type-cache nil
  "Cache of type information (a hash table).")

(defvar-local cape-tidal--filter-installed nil
  "Whether the output filter has been installed.")

;;; Utility functions

(defun cape-tidal--in-string-or-comment-p ()
  "Return non-nil if point is inside a string or a comment."
  (let ((state (syntax-ppss)))
    (or (nth 3 state)   ; inside a string
        (nth 4 state)))) ; inside a comment

(defun cape-tidal--grab-symbol ()
  "Return the symbol at point.
The value is a cons (start . end), or nil if there is no symbol."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (cons (car bounds) (cdr bounds)))))

(defun cape-tidal--tidal-buffer ()
  "Return the Tidal comint buffer, or nil if it does not exist."
  (and (boundp 'tidal-buffer)
       (buffer-live-p (get-buffer tidal-buffer))
       (get-buffer tidal-buffer)))

(defun cape-tidal--live-process (tidal-buf)
  "Return the live process of TIDAL-BUF, or nil if it is dead.
tidal-send-string throws an error when the process is dead, so always check
before sending."
  (let ((proc (get-buffer-process tidal-buf)))
    (and proc (process-live-p proc) proc)))

(defun cape-tidal--send-string (str)
  "Send STR to the Tidal process."
  (when (fboundp 'tidal-send-string)
    (tidal-send-string str)))

;;; GHCi communication

(defun cape-tidal--cancel-timeout ()
  "Cancel the timeout timer."
  (when cape-tidal--timeout-timer
    (cancel-timer cape-tidal--timeout-timer)
    (setq cape-tidal--timeout-timer nil)))

(defun cape-tidal--reset-state ()
  "Reset the current request's state back to idle (keep the pending)."
  (cape-tidal--cancel-timeout)
  (setq cape-tidal--state nil)
  (setq cape-tidal--output nil)
  (setq cape-tidal--callback nil)
  (setq cape-tidal--deadline nil)
  (setq cape-tidal--interrupted nil))

(defun cape-tidal--drop-pending ()
  "Discard the pending request and pass nil to its callback."
  (when cape-tidal--pending
    (let ((callback (nth 1 cape-tidal--pending)))
      (setq cape-tidal--pending nil)
      (when callback (funcall callback nil)))))

(defun cape-tidal--hard-reset ()
  "Reset the request state, pending, foreign-send counter, and any running
  prefetch.
The current buffer must be the tidal buffer.  The type cache is left alone
\(whether to clear it is the caller's decision)."
  (cape-tidal--reset-state)
  (cape-tidal--drop-pending)
  (setq cape-tidal--foreign-pending 0)
  (setq cape-tidal--last-foreign-time nil)
  (setq cape-tidal--in-foreign-block nil)
  (setq cape-tidal--idle-tail ""
        cape-tidal--idle-tail-bol t)
  (cape-tidal--prefetch-cancel)
  (setq cape-tidal--prefetch-generation (1+ cape-tidal--prefetch-generation)))

(defun cape-tidal--handle-timeout ()
  "Handle a timeout.  The current buffer must be the tidal buffer.
The response may still be on the wire, so instead of returning to passthrough
immediately, move to `draining' and swallow (with a deadline) up to the next
prompt boundary.  If the process is dead no response is coming, so clear
everything."
  (let ((callback (and (eq cape-tidal--state 'active) cape-tidal--callback)))
    (if (cape-tidal--live-process (current-buffer))
        (progn
          (cape-tidal--cancel-timeout)
          (setq cape-tidal--callback nil)
          (setq cape-tidal--state 'draining)
          (setq cape-tidal--deadline (+ (float-time) cape-tidal-drain-timeout)))
      (cape-tidal--reset-state)
      (cape-tidal--drop-pending))
    (when callback
      (message "cape-tidal: GHCi response timeout")
      (funcall callback nil))))

(defun cape-tidal--find-boundary (str)
  "Return the end position of the first prompt boundary in STR, or nil if none.
The boundary includes the whitespace (other than newline) that follows the
prompt.  GHCi's prompt \"tidal> \" is printed with its trailing space and no
newline, so leaving the space would leak it into comint as a remainder."
  (when (and str (string-match cape-tidal-prompt-regexp str))
    (let ((end (match-end 0)))
      (while (and (< end (length str))
                  (memq (aref str end) '(?\s ?\t)))
        (setq end (1+ end)))
      end)))

(defun cape-tidal--fail-open (output)
  "Handle a suppression deadline expiring (fail open).
Clear all state and pending and return OUTPUT to the raw comint.  In a
user-interrupt-driven drain the accumulated output may contain the user's
evaluation result, so the accumulated part is returned too.  Otherwise the
accumulated part is a fragment of our own response, so discard it."
  (let ((acc cape-tidal--output)
        (interrupted cape-tidal--interrupted)
        (callback (and (eq cape-tidal--state 'active) cape-tidal--callback)))
    (cape-tidal--reset-state)
    (cape-tidal--drop-pending)
    (when callback (funcall callback nil))
    (if (and interrupted acc)
        (concat acc output)
      output)))

(defun cape-tidal--clear-to-send-p ()
  "Return non-nil if no foreign response is pending and it is OK to send.
The current buffer must be the tidal buffer.  So the counter cannot get stuck
on
a missed prompt (chunk splitting, etc.), reset it to 0 to recover once enough
time has passed since the last foreign send."
  (when (and (> cape-tidal--foreign-pending 0)
             cape-tidal--last-foreign-time
             (> (- (float-time) cape-tidal--last-foreign-time)
                (* 2 cape-tidal-drain-timeout)))
    ;; Fail open: drop all the state we lost track of.  Keeping only the block
    ;; state would make us stop counting every later foreign send forever.
    (setq cape-tidal--foreign-pending 0)
    (setq cape-tidal--in-foreign-block nil)
    (setq cape-tidal--idle-tail ""
          cape-tidal--idle-tail-bol t)
    (setq cape-tidal--last-foreign-done (float-time)))
  (zerop cape-tidal--foreign-pending))

(defun cape-tidal--note-foreign-send (&optional count)
  "Record a send to the tidal process by something other than cape-tidal
  (evaluation, etc.).
COUNT is the number of prompts GHCi returns for that send (1 if omitted).  0
means do nothing.  A completion in progress loses immediately: an `active' one
is demoted to a short-deadline drain, its callback is passed nil, and the
pending
is discarded.  The type prefetch is not discarded but paused — while
`cape-tidal--clear-to-send-p' is nil a step just reschedules instead of
sending,
so it resumes on its own once the evaluation output has cleared.
Design principle from the second flight on: when things are ambiguous
cape-tidal
loses and never hides the playing output."
  (let ((tidal-buf (cape-tidal--tidal-buffer))
        (count (or count 1)))
    (when (and tidal-buf (> count 0))
      (with-current-buffer tidal-buf
        (setq cape-tidal--foreign-pending (+ cape-tidal--foreign-pending count))
        (setq cape-tidal--last-foreign-time (float-time))
        (cape-tidal--drop-pending)
        (cond
         ((eq cape-tidal--state 'active)
          (let ((callback cape-tidal--callback))
            (cape-tidal--cancel-timeout)
            (setq cape-tidal--callback nil)
            (setq cape-tidal--state 'draining)
            (setq cape-tidal--interrupted t)
            (setq cape-tidal--deadline
                  (+ (float-time) cape-tidal-interrupt-drain-timeout))
            (when callback (funcall callback nil))))
         ((eq cape-tidal--state 'draining)
          (setq cape-tidal--interrupted t)
          (setq cape-tidal--deadline
                (min cape-tidal--deadline
                     (+ (float-time) cape-tidal-interrupt-drain-timeout)))))))))

(defun cape-tidal--foreign-send-count (s)
  "Return the number of prompts GHCi returns for foreign send S, and update the
  block state.
S may contain newlines (GHCi receives it a line at a time).  A top-level line
returns one prompt.  Lines from `:{' until `:}' return no prompt, and one
prompt
is returned after `:}' (`prompt-cont' is empty).  The whole block is counted as
1 at the `:{'.  A stray `:}' outside a block makes GHCi return an error and a
prompt, so it counts as an ordinary line.  A `:{' inside a block is body.
The current buffer must be the tidal buffer."
  (let ((n 0))
    (dolist (line (split-string (or s "") "\n"))
      (let ((trimmed (string-trim line)))
        (cond
         (cape-tidal--in-foreign-block
          (when (equal trimmed ":}")
            (setq cape-tidal--in-foreign-block nil)))
         ((equal trimmed ":{")
          (setq cape-tidal--in-foreign-block t)
          (setq n (1+ n)))
         (t (setq n (1+ n))))))
    n))

(defun cape-tidal--before-tidal-send (&rest args)
  "The :before advice on `tidal-send-string'.  Detect a send from elsewhere.
The first element of ARGS is the string being sent."
  (unless cape-tidal--own-send
    (let ((tidal-buf (cape-tidal--tidal-buffer)))
      (when tidal-buf
        (let ((n (with-current-buffer tidal-buf
                   (cape-tidal--foreign-send-count (car args)))))
          (cape-tidal--note-foreign-send n))))))

(defun cape-tidal--comint-input-noticed (input)
  "Treat manual INPUT in the comint buffer as a foreign send.
Called in the tidal buffer from `comint-input-filter-functions'.  A manual
`:{' … `:}' goes through the same block state machine as one sent via
`tidal-send-string'."
  (cape-tidal--note-foreign-send (cape-tidal--foreign-send-count input)))

(defun cape-tidal--note-idle-output (output)
  "Watch OUTPUT passed through during idle and count completed foreign sends.
Once all the prompts for the foreign sends have gone by, send the completion
pending that was waiting.  The current buffer must be the tidal buffer.

So a prompt split across a chunk boundary is not missed, concatenate with the
previous trailing fragment (`cape-tidal--idle-tail') before matching.  Each
time
a prompt is counted, drop the horizontal whitespace right after it (the
trailing
part of `tidal> ' itself) and rescan the rest as a `virtual' line beginning
within the same call — so that `tidal> tidal> ' in one chunk still counts as
two.  The fragment keeps only the part after the last counted prompt, so the
same
prompt is never counted twice."
  (if (<= cape-tidal--foreign-pending 0)
      (setq cape-tidal--idle-tail ""
            cape-tidal--idle-tail-bol t)
    (let ((text (concat cape-tidal--idle-tail output))
          ;; If the fragment starts mid-line, put a sentinel so `^' does not
          ;; hold at its start.
          (sentinel (not cape-tidal--idle-tail-bol))
          (count 0)
          (bol cape-tidal--idle-tail-bol))
      (when sentinel (setq text (concat "\0" text)))
      (let ((done nil))
        (while (and (not done) (string-match cape-tidal-prompt-regexp text))
          (let ((mend (match-end 0)))
            (if (zerop mend)
                ;; A regexp that matches the empty string would loop forever,
                ;; so bail.
                (setq done t)
              (setq count (1+ count))
              ;; From right after the counted prompt, drop horizontal
              ;; whitespace
              ;; to reach a virtual line beginning.
              (setq text (string-trim-left (substring text mend) "[ \t]+"))
              (setq sentinel nil)
              (setq bol t)))))
      ;; Remove the sentinel if it is still there (nothing was counted).
      (when (and sentinel (> (length text) 0) (eq (aref text 0) 0))
        (setq text (substring text 1)))
      ;; Keep the tail for the next match.  When truncating, cut at a line
      ;; beginning if possible.
      (if (<= (length text) cape-tidal--idle-tail-max)
          (setq cape-tidal--idle-tail text
                cape-tidal--idle-tail-bol bol)
        (let* ((window (substring text (- (length text) cape-tidal--idle-tail-max)))
               (nl (string-match "\n" window)))
          (if nl
              (setq cape-tidal--idle-tail (substring window (1+ nl))
                    cape-tidal--idle-tail-bol t)
            (setq cape-tidal--idle-tail window
                  cape-tidal--idle-tail-bol nil))))
      (when (> count 0)
        (setq cape-tidal--foreign-pending
              (max 0 (- cape-tidal--foreign-pending count)))
        (when (zerop cape-tidal--foreign-pending)
          (setq cape-tidal--idle-tail ""
                cape-tidal--idle-tail-bol t)
          (setq cape-tidal--last-foreign-done (float-time))
          (cape-tidal--flush-pending))))))

(defun cape-tidal--preoutput-filter (output)
  "comint's output filter.  OUTPUT is the output string from GHCi.
When idle, pass through.  During a request, take everything up to the prompt
boundary as one response, and return the remainder after the boundary to
comint,
since it is not addressed to us.  If the state's deadline has passed, lift all
suppression (fail open)."
  (cond
   ;; Idle: pass through (only count completed foreign-send responses).
   ((null cape-tidal--state)
    (cape-tidal--note-idle-output output)
    output)
   ;; Deadline passed: fail open.
   ((> (float-time) cape-tidal--deadline)
    (cape-tidal--fail-open output))
   (t
    (setq cape-tidal--output (concat cape-tidal--output output))
    (let ((boundary (cape-tidal--find-boundary cape-tidal--output)))
      (if (not boundary)
          ;; Boundary not reached: accumulate and keep suppressing.
          ""
        (let ((response (substring cape-tidal--output 0 boundary))
              (remainder (substring cape-tidal--output boundary))
              (callback (and (eq cape-tidal--state 'active)
                             cape-tidal--callback)))
          ;; Finalize the current request and go idle.  When draining, discard
          ;; response.
          (cape-tidal--reset-state)
          ;; Send the completion pending first (before the type prefetch that
          ;; starts inside the callback).
          (cape-tidal--flush-pending)
          (when callback (funcall callback response))
          ;; The remainder is not from the current request (the next request is
          ;; sent after this point, so it is not that response either).  It may
          ;; contain a foreign-send response, so count its prompts as
          ;; passthrough.
          (unless (string-empty-p remainder)
            ;; The remainder starts right after the boundary prompt (= at a
            ;; line beginning).
            (setq cape-tidal--idle-tail ""
                  cape-tidal--idle-tail-bol t)
            (cape-tidal--note-idle-output remainder))
          remainder))))))

(defun cape-tidal--timer-fired (tidal-buf id)
  "Handle the timeout timer firing.
Acts only when TIDAL-BUF is live, ID matches the current request, and the state
is `active'.  If it does not match, it is an old timer that escaped
cancellation, so do nothing."
  (when (buffer-live-p tidal-buf)
    (with-current-buffer tidal-buf
      (when (and (eql id cape-tidal--request-id)
                 (eq cape-tidal--state 'active))
        (cape-tidal--handle-timeout)))))

(defun cape-tidal--transmit (request callback)
  "Send REQUEST and enter the `active' state.
The current buffer must be the tidal buffer and the state must be idle.
If the send fails, revert the state and pass nil to CALLBACK."
  (setq cape-tidal--request-id (1+ cape-tidal--request-id))
  (setq cape-tidal--state 'active)
  (setq cape-tidal--output nil)
  (setq cape-tidal--callback callback)
  (setq cape-tidal--interrupted nil)
  (setq cape-tidal--deadline (+ (float-time) cape-tidal-timeout))
  (setq cape-tidal--timeout-timer
        (run-with-timer cape-tidal-timeout nil
                        #'cape-tidal--timer-fired
                        (current-buffer) cape-tidal--request-id))
  ;; tidal-send-string throws an error when the process is dead.
  ;; Set own-send so the advice excludes our own send from foreign-send
  ;; detection.
  (condition-case err
      (let ((cape-tidal--own-send t))
        (cape-tidal--send-string request))
    (error
     (cape-tidal--reset-state)
     (message "cape-tidal: send failed: %s" (error-message-string err))
     (funcall callback nil))))

(defun cape-tidal--flush-pending ()
  "Send the pending request if idle and no foreign response is pending.
The current buffer must be the tidal buffer.  A stale pending that has sat for
longer than `cape-tidal-timeout' since it was enqueued is discarded without
being sent."
  (when (and cape-tidal--pending
             (null cape-tidal--state)
             (cape-tidal--clear-to-send-p))
    (let ((request (nth 0 cape-tidal--pending))
          (callback (nth 1 cape-tidal--pending))
          (enqueued (nth 2 cape-tidal--pending)))
      (setq cape-tidal--pending nil)
      (if (> (- (float-time) enqueued) cape-tidal-timeout)
          (funcall callback nil)
        (cape-tidal--transmit request callback)))))

(defun cape-tidal--send-request (request callback &optional queue)
  "Send REQUEST to GHCi and pass the result to CALLBACK.
REQUEST is the string to send; CALLBACK receives the result.  While another
request is in progress, if QUEUE is non-nil enqueue into the pending (one slot,
latest wins) to be sent after the boundary is reached, and if nil pass nil to
CALLBACK immediately (completion is called with queue, the type prefetch
without).  CALLBACK is also passed nil when there is no tidal buffer or
process,
or the send fails."
  (let* ((tidal-buf (cape-tidal--tidal-buffer))
         (proc (and tidal-buf (cape-tidal--live-process tidal-buf))))
    (if (not proc)
        (funcall callback nil)
      ;; Ensure setup even for a call that does not go through capf (e.g. GHCi
      ;; restarted in the middle of a prefetch chain).
      (cape-tidal--ensure-filter-installed)
      (with-current-buffer tidal-buf
        (if (or cape-tidal--state
                ;; Do not send while waiting on a foreign send's (evaluation)
                ;; response either.
                (not (cape-tidal--clear-to-send-p)))
            (if queue
                (progn
                  (cape-tidal--drop-pending)
                  (setq cape-tidal--pending
                        (list request callback (float-time))))
              (funcall callback nil))
          (cape-tidal--transmit request callback))))))

;;; Output parsing

(defun cape-tidal--strip-prompt (line)
  "Strip a GHCi prompt from the beginning of LINE if present, and return it."
  (if (and (string-match cape-tidal-prompt-regexp line)
           (= (match-beginning 0) 0))
      (substring line (match-end 0))
    line))

(defconst cape-tidal--char-escapes
  '((?a . 7) (?b . 8) (?f . 12) (?n . 10) (?r . 13) (?t . 9) (?v . 11)
    (?\\ . ?\\) (?\" . ?\") (?\' . ?\'))
  "Haskell single-character escapes.  The key is the character after the backslash.")

(defconst cape-tidal--ascii-escapes
  '(("NUL" . 0) ("SOH" . 1) ("STX" . 2) ("ETX" . 3) ("EOT" . 4) ("ENQ" . 5)
    ("ACK" . 6) ("BEL" . 7) ("DLE" . 16) ("DC1" . 17) ("DC2" . 18)
    ("DC3" . 19) ("DC4" . 20) ("NAK" . 21) ("SYN" . 22) ("ETB" . 23)
    ("CAN" . 24) ("SUB" . 26) ("ESC" . 27) ("DEL" . 127)
    ("BS" . 8) ("HT" . 9) ("LF" . 10) ("VT" . 11) ("FF" . 12) ("CR" . 13)
    ("SO" . 14) ("SI" . 15) ("EM" . 25) ("FS" . 28) ("GS" . 29)
    ("RS" . 30) ("US" . 31) ("SP" . 32))
  "Haskell ASCII mnemonic escapes.
Longest match is required, so the 3-letter ones come first (so `\\SOH' is not
misread as `\\SO').")

(defun cape-tidal--digit-value (ch radix)
  "Return the value of CH if it is a digit in RADIX, otherwise nil."
  (let ((v (cond ((and (>= ch ?0) (<= ch ?9)) (- ch ?0))
                 ((and (>= ch ?a) (<= ch ?f)) (+ 10 (- ch ?a)))
                 ((and (>= ch ?A) (<= ch ?F)) (+ 10 (- ch ?A)))
                 (t nil))))
    (and v (< v radix) v)))

(defun cape-tidal--scan-radix (body i radix)
  "Read a RADIX number from position I in BODY, and return (CHAR . NEXT-INDEX).
nil if unreadable."
  (let ((start i)
        (n (length body)))
    (while (and (< i n) (cape-tidal--digit-value (aref body i) radix))
      (setq i (1+ i)))
    (when (> i start)
      (let ((v (string-to-number (substring body start i) radix)))
        (and (<= 0 v) (<= v (max-char)) (cons v i))))))

(defun cape-tidal--match-mnemonic (body i)
  "Read an ASCII mnemonic from position I in BODY by longest match.
Return (CHAR . NEXT-INDEX)."
  (let ((n (length body))
        (found nil))
    (dolist (entry cape-tidal--ascii-escapes)
      (unless found
        (let* ((name (car entry))
               (stop (+ i (length name))))
          (when (and (<= stop n) (string= name (substring body i stop)))
            (setq found (cons (cdr entry) stop))))))
    found))

(defun cape-tidal--decode-ghci-string (line)
  "Decode LINE, a Haskell string literal GHCi printed with `show'.
Return nil if the whole line is not quoted, or contains an escape that cannot
be
interpreted (the caller treats the whole response as failed).

Merely stripping the quotes with `string-trim' is not enough.  For example the
set-difference operator of `Data.List' is the two characters \\\\, but arrives
from GHCi as the six-character line \"\\\\\\\\\".  A non-ASCII identifier
arrives
as a decimal escape like `\\955', so silently turning an unknown escape into
some
other character is dangerous and must fail.

A string gap (`\\' whitespace `\\') does not appear in `show' output, so it is
not supported."
  (let ((len (length line)))
    (when (and (>= len 2)
               (eq (aref line 0) ?\")
               (eq (aref line (1- len)) ?\"))
      (let* ((body (substring line 1 (1- len)))
             (n (length body))
             (out nil)
             (i 0)
             (bad nil))
        (while (and (not bad) (< i n))
          (let ((ch (aref body i)))
            (cond
             ;; A raw quote does not appear in `show' output (always becomes
             ;; \").
             ((eq ch ?\") (setq bad t))
             ((not (eq ch ?\\)) (push ch out) (setq i (1+ i)))
             (t
              (setq i (1+ i))
              (if (>= i n)
                  (setq bad t)
                (let* ((c (aref body i))
                       (simple (assq c cape-tidal--char-escapes)))
                  (cond
                   (simple (push (cdr simple) out) (setq i (1+ i)))
                   ;; \& is the empty string.  Used to separate a numeric
                   ;; escape
                   ;; from a following digit.
                   ((eq c ?&) (setq i (1+ i)))
                   ((eq c ?^)
                    (setq i (1+ i))
                    (if (>= i n)
                        (setq bad t)
                      (let ((k (aref body i)))
                        (if (and (>= k ?@) (<= k ?_))
                            (progn (push (- k ?@) out) (setq i (1+ i)))
                          (setq bad t)))))
                   ((memq c '(?x ?o))
                    (let ((r (cape-tidal--scan-radix
                              body (1+ i) (if (eq c ?x) 16 8))))
                      (if r
                          (progn (push (car r) out) (setq i (cdr r)))
                        (setq bad t))))
                   ((cape-tidal--digit-value c 10)
                    (let ((r (cape-tidal--scan-radix body i 10)))
                      (if r
                          (progn (push (car r) out) (setq i (cdr r)))
                        (setq bad t))))
                   (t
                    (let ((m (cape-tidal--match-mnemonic body i)))
                      (if m
                          (progn (push (car m) out) (setq i (cdr m)))
                        (setq bad t)))))))))))
        (unless bad
          (apply #'string (nreverse out)))))))

(defun cape-tidal--encode-ghci-string (str)
  "Convert STR into a Haskell string literal (with quotes) to pass to GHCi.
Embedding `\\' or `\"' as-is breaks the request.  This applies the moment you
try
to complete Haskell's `\\' operator, and a newline mixed in would break the
command boundary."
  (let ((out nil))
    (dolist (ch (append str nil))
      (cond
       ((eq ch ?\\) (push ?\\ out) (push ?\\ out))
       ((eq ch ?\") (push ?\\ out) (push ?\" out))
       ((eq ch ?\n) (push ?\\ out) (push ?n out))
       ((eq ch ?\t) (push ?\\ out) (push ?t out))
       (t (push ch out))))
    (concat "\"" (apply #'string (nreverse out)) "\"")))

(defun cape-tidal--parse-complete-output (output)
  "Parse the output of `:complete repl` and return a response plist.
On success (:ok t :printed N :total M :common-prefix S :candidates LIST), on
failure (:ok nil).

Failure must always be distinguished from a legitimate 0 results.  Mixing them
would cache a communication failure as `it is settled that there are no
candidates', killing completion for the rest of the session.

The header line is the triple `<printed> <total> <common-prefix>'.  Per the GHC
documentation, common-prefix is a string prepended to each candidate to form
the
post-completion text; it is not part of the candidates themselves (empty in the
usual case of completing a single symbol)."
  (let ((fail (list :ok nil)))
    (if (null output)
        fail
      (let* ((lines (split-string output "\n" t))
             ;; Do not drop a prompt line whole; strip only the part at the
             ;; line
             ;; beginning.  Dropping the whole line loses the header when the
             ;; prompt and header share a line, as in `tidal> 100 120 ""', and
             ;; everything after is off by one.
             (lines (mapcar #'cape-tidal--strip-prompt lines))
             (lines (seq-remove
                     (lambda (line) (string-empty-p (string-trim line)))
                     lines))
             (header (and lines (string-trim (car lines))))
             (body (cdr lines)))
        (if (or (null header)
                (not (string-match
                      "\\`\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(\".*\"\\)\\'"
                      header)))
            fail
          (let ((printed (string-to-number (match-string 1 header)))
                (total (string-to-number (match-string 2 header)))
                (common (cape-tidal--decode-ghci-string (match-string 3 header))))
            ;; Do not trust a response whose line count disagrees with the
            ;; header
            ;; (detects dropped or crossed responses).  printed > total is
            ;; impossible in the protocol, so treat it as a broken response.
            (if (or (null common)
                    (> printed total)
                    (/= (length body) printed))
                fail
              (let ((cands nil)
                    (bad nil))
                (dolist (line body)
                  (let ((decoded (cape-tidal--decode-ghci-string (string-trim line))))
                    (if (null decoded)
                        (setq bad t)
                      (push (if (string-empty-p common)
                                decoded
                              (concat common decoded))
                            cands))))
                (if bad
                    fail
                  (list :ok t
                        :printed printed
                        :total total
                        :common-prefix common
                        :candidates (delete-dups (nreverse cands))))))))))))

(defun cape-tidal--parse-type-output (output)
  "Parse the output of `:type` and return the type information."
  (when output
    (let* ((lines (split-string output "\n" t))
           ;; Remove prompt lines.
           (lines (seq-remove
                   (lambda (line)
                     (string-match-p cape-tidal-prompt-regexp line))
                   lines))
           ;; Join, collapsing the runs of whitespace from continuation-line
           ;; indentation into one.
           (type-str (replace-regexp-in-string
                      "[ \t]+" " " (string-join lines " ")))
           ;; Split on :: and remove the function name.
           (parts (split-string type-str "::" t))
           (type-part (if (cdr parts)
                          (string-trim (string-join (cdr parts) "::"))
                        nil)))
      (when (and type-part (not (string-empty-p type-part)))
        (concat ":: " type-part)))))

;;; Completion

(defun cape-tidal--get-candidates (prefix callback)
  "Fetch completion candidates for PREFIX asynchronously."
  ;; Do not add a trailing newline.  tidal-send-string appends "\n", so adding
  ;; one here too would send an extra blank line to GHCi, get two prompt
  ;; responses, and misalign responses with callbacks (the cause of candidates
  ;; leaking into comint).
  (let ((request (format ":complete repl %d %s"
                         cape-tidal-candidates-limit
                         (cape-tidal--encode-ghci-string prefix))))
    ;; Completion is user-initiated, so if busy, enqueue into the pending
    ;; (queue = t).
    (cape-tidal--send-request
     request
     (lambda (output)
       (funcall callback (cape-tidal--parse-complete-output output)))
     t)))

(defun cape-tidal--get-type (symbol callback)
  "Fetch the type information for SYMBOL asynchronously."
  (let ((tidal-buf (cape-tidal--tidal-buffer)))
    (when tidal-buf
      ;; Check the cache.
      (with-current-buffer tidal-buf
        (unless cape-tidal--type-cache
          (setq cape-tidal--type-cache (make-hash-table :test 'equal)))
        (let ((cached (gethash symbol cape-tidal--type-cache 'cape-tidal--miss)))
          (cond
           ((stringp cached) (funcall callback cached))
           ;; A negative-cache entry (a keyword etc. that `:type' does not
           ;; accept) returns nil.
           ((not (eq cached 'cape-tidal--miss)) (funcall callback nil))
           (t
            ;; Not in the cache, so fetch (the trailing newline is added by
            ;; tidal-send-string).
            (let ((request (format ":type (%s)" symbol)))
              (cape-tidal--send-request
               request
               (lambda (output)
                 (let ((type-info (cape-tidal--parse-type-output output)))
                   ;; When output is nil (timeout, send failure) do not cache.
                   ;; Negative-cache only those where a response arrived but no
                   ;; type was found.
                   (when output
                     (puthash symbol (or type-info 'cape-tidal--none)
                              cape-tidal--type-cache))
                   (funcall callback type-info))))))))))))

;;; Background type fetching

(defun cape-tidal--type-known-p (candidate tidal-buf)
  "Return non-nil if CANDIDATE's type is in TIDAL-BUF's cache.
Includes the negative cache."
  (let ((cache (buffer-local-value 'cape-tidal--type-cache tidal-buf)))
    (and cache
         (not (eq (gethash candidate cache 'cape-tidal--miss)
                  'cape-tidal--miss)))))

(defun cape-tidal--prefetch-cancel ()
  "Cancel the scheduled prefetch step and drop the queue, demand, and budget.
The current buffer must be the tidal buffer."
  (when cape-tidal--prefetch-timer
    (cancel-timer cape-tidal--prefetch-timer)
    (setq cape-tidal--prefetch-timer nil))
  (setq cape-tidal--prefetch-queue nil)
  (setq cape-tidal--prefetch-demand nil)
  (setq cape-tidal--prefetch-budget 0))

(defun cape-tidal--prefetch-schedule ()
  "Schedule the next prefetch step `cape-tidal-prefetch-delay' seconds later.
Do nothing if one is already scheduled.  The current buffer must be the tidal
buffer."
  (unless cape-tidal--prefetch-timer
    (setq cape-tidal--prefetch-timer
          (run-with-timer cape-tidal-prefetch-delay nil
                          #'cape-tidal--prefetch-step
                          (current-buffer) cape-tidal--prefetch-generation))))

(defun cape-tidal--within-quiet-period-p ()
  "Return non-nil if less than `cape-tidal-prefetch-delay' seconds have passed
  since the last foreign send finished.
When an evaluation arrives and clears after the timer was scheduled, the
interval
from the scheduling time is not `the quiet delay since the evaluation
finished',
so measure from the finish time again."
  (and cape-tidal--last-foreign-done
       (< (- (float-time) cape-tidal--last-foreign-done)
          cape-tidal-prefetch-delay)))

(defun cape-tidal--prefetch-next-candidate ()
  "Take the next candidate to fetch.
Prefer candidates whose display was requested (`cape-tidal--prefetch-demand')
in
display order, otherwise the front of the queue.  Skip already-fetched ones
(including the negative cache) without spending budget.
The current buffer must be the tidal buffer."
  (let ((found nil))
    (while (and (not found)
                (or cape-tidal--prefetch-demand cape-tidal--prefetch-queue))
      (let ((c (if cape-tidal--prefetch-demand
                   (pop cape-tidal--prefetch-demand)
                 (pop cape-tidal--prefetch-queue))))
        (unless (cape-tidal--type-known-p c (current-buffer))
          (setq found c))))
    found))

(defun cape-tidal--prefetch-step (tidal-buf gen)
  "Fetch the type of one candidate and schedule the next.
If GHCi is busy, an evaluation just finished, or the user is typing, reschedule
instead of sending (pause).  A previous implementation let the chain vanish
here,
so completing while playing would leave type fetching dead in the middle until
the next completion.  If GEN differs from the current generation (another
completion session has started), do nothing."
  (when (buffer-live-p tidal-buf)
    (with-current-buffer tidal-buf
      (when (eql gen cape-tidal--prefetch-generation)
        ;; Clear the slot only after checking the generation, so an old
        ;; generation's timer running late does not erase a new schedule.
        (setq cape-tidal--prefetch-timer nil)
        (cond
         ;; Termination conditions.
         ((or (<= cape-tidal--prefetch-budget 0)
              (not (cape-tidal--live-process tidal-buf))
              (and (null cape-tidal--prefetch-demand)
                   (null cape-tidal--prefetch-queue)))
          nil)
         ;; Pause: resume once things go quiet.
         ((or cape-tidal--state
              cape-tidal--pending
              (not (cape-tidal--clear-to-send-p))
              (cape-tidal--within-quiet-period-p)
              (input-pending-p))
          (cape-tidal--prefetch-schedule))
         (t
          (let ((candidate (cape-tidal--prefetch-next-candidate)))
            ;; Stop if no unfetched candidate remains.
            (when candidate
              (setq cape-tidal--prefetch-budget
                    (1- cape-tidal--prefetch-budget))
              (cape-tidal--get-type
               candidate
               (lambda (type)
                 ;; Once a type is attached, reflect it in the popup on screen.
                 (when (stringp type)
                   (cape-tidal--schedule-popup-refresh))
                 (when (buffer-live-p tidal-buf)
                   (with-current-buffer tidal-buf
                     (when (eql gen cape-tidal--prefetch-generation)
                       ;; A candidate whose type was not obtained (not even in
                       ;; the
                       ;; negative cache) because an evaluation cut in is put
                       ;; back
                       ;; at the front.  The budget is not refunded — the
                       ;; budget
                       ;; is the actual number of sends, so it always stops
                       ;; after
                       ;; a finite count.
                       (unless (cape-tidal--type-known-p candidate tidal-buf)
                         (push candidate cape-tidal--prefetch-demand))
                       ;; Do not send the next one immediately; reschedule (a
                       ;; gap
                       ;; for an evaluation to cut in).
                       (cape-tidal--prefetch-schedule))))))))))))))

(defun cape-tidal--refresh-popup ()
  "Redraw the completion popup, only while Corfu is showing.
Corfu redraws only on `post-command-hook', so an annotation that arrives
asynchronously is invisible until the user presses something.  Use
`corfu--candidates-popup', not `corfu--exhibit' — the latter also makes
decisions such as `finish the completion if the sole candidate exactly matches
the input', which is not something to run from a timer without a user action.
The former only re-annotates the existing candidates and redraws, without that
decision.

The check starts from the buffer of the selected window and is done inside it.
The first element of `completion-in-region--data' is a marker on some Corfu
versions and an integer on others, so decide by the end position (a marker)
pointing at this buffer.  If input is pending, wait a little and retry
(dropping
it once means the type stays invisible until the next redraw).  Do not touch
other completion UIs (they appear on their next redraw).  This depends on a
private function, so do nothing silently if it is absent.  Errors are surfaced
with `message'."
  (setq cape-tidal--refresh-timer nil)
  (with-current-buffer (window-buffer (selected-window))
    (let* ((data completion-in-region--data)
           (beg (and (consp data) (nth 0 data)))
           (end (and (consp data) (nth 1 data)))
           (session (and completion-in-region-mode
                         (bound-and-true-p corfu-mode)
                         (bound-and-true-p corfu--candidates)
                         (fboundp 'corfu--candidates-popup)
                         (markerp end)
                         (eq (marker-buffer end) (current-buffer))
                         (number-or-marker-p beg)
                         (<= beg (point))
                         (<= (point) end)))
           (busy (input-pending-p)))
      (when cape-tidal--debug
        (message "cape-tidal refresh: buf=%s cir=%S corfu=%S cands=%S data=%S session=%S busy=%S"
                 (buffer-name) completion-in-region-mode
                 (bound-and-true-p corfu-mode)
                 (and (bound-and-true-p corfu--candidates) t)
                 (and (consp data) (list (nth 0 data) (nth 1 data)))
                 (and session t) busy))
      (cond
       ((not session) nil)
       (busy
        ;; If the user is typing, it will be drawn on post-command.  For the
        ;; case
        ;; where we skipped due to other input (frame events, etc.), retry a
        ;; little.
        (when (< cape-tidal--refresh-retries 3)
          (setq cape-tidal--refresh-retries (1+ cape-tidal--refresh-retries))
          (cape-tidal--schedule-popup-refresh)))
       (t
        (setq cape-tidal--refresh-retries 0)
        (condition-case err
            (let ((pos (posn-at-point
                        (+ beg (length (or (bound-and-true-p corfu--base) ""))))))
              (when cape-tidal--debug
                (message "cape-tidal refresh: posn=%S -> redraw" (and pos t)))
              (when pos
                (corfu--candidates-popup pos)))
          ;; A `quit' from `while-no-input' means "input arrived, so drawing
          ;; was given up".
          (quit nil)
          (error (message "cape-tidal: popup refresh failed: %s"
                          (error-message-string err)))))))))

(defun cape-tidal--schedule-popup-refresh ()
  "A type arrived, so schedule a redraw of the completion UI.  Several arrivals
  become one.
Uses an ordinary timer.  An idle timer's firing conditions when it is `already
been idle for a while' are hard to reason about, and it is unsuited to this use
where scheduling happens from inside process-output handling (= while idle)."
  (when cape-tidal--debug
    (message "cape-tidal refresh: scheduled (timer=%S)" (and cape-tidal--refresh-timer t)))
  (unless cape-tidal--refresh-timer
    (setq cape-tidal--refresh-timer
          (run-with-timer 0.05 nil #'cape-tidal--refresh-popup))))

(defun cape-tidal--fetch-types-async (candidates)
  "Start fetching the type information for CANDIDATES in the background.
Advance the generation as a new completion session and replace the old queue.
The number of sends is capped by `cape-tidal-prefetch-limit' as a budget.  All
candidates go in the queue; `cape-tidal--annotation-function' moves candidates
whose display is requested into the demand FIFO, so the budget is spent on the
candidates on screen in display order."
  (let ((tidal-buf (cape-tidal--tidal-buffer)))
    (when tidal-buf
      (with-current-buffer tidal-buf
        (cape-tidal--prefetch-cancel)
        (setq cape-tidal--prefetch-generation
              (1+ cape-tidal--prefetch-generation))
        (when candidates
          ;; The demand hint removes destructively, so do not share the
          ;; caller's list.
          (setq cape-tidal--prefetch-queue (copy-sequence candidates))
          (setq cape-tidal--prefetch-budget cape-tidal-prefetch-limit)
          (cape-tidal--prefetch-schedule))))))

(defun cape-tidal--demand-reset (tidal-buf)
  "Clear the demand hints and return the not-yet-fetched candidates to the
  ordinary queue.
When the input changes the candidates on screen change too.  The demand for the
candidates visible under the previous input is dropped, but the candidates
themselves are not lost — `cape-tidal--prioritize-prefetch' removes a candidate
from the queue when it moves it to demand, so simply setting demand to nil
would
mean that candidate is never fetched again.  Return it at low priority (the
tail
of the queue)."
  (with-current-buffer tidal-buf
    (when cape-tidal--prefetch-demand
      (setq cape-tidal--prefetch-queue
            (append cape-tidal--prefetch-queue cape-tidal--prefetch-demand))
      (setq cape-tidal--prefetch-demand nil))))

(defun cape-tidal--prioritize-prefetch (candidate tidal-buf)
  "If CANDIDATE is in the prefetch queue, move it to the tail of the demand
  FIFO (a demand hint).
Does not send to GHCi, wait synchronously, or create a timer.  A candidate not
in
the queue (already moved to demand, or from another session) is ignored.
Appending at the tail means the order in which Corfu annotates the visible
range
top to bottom = display order becomes the fetch order.
The queue length is at most `cape-tidal-candidates-limit', so a linear scan is
enough."
  (with-current-buffer tidal-buf
    (when (member candidate cape-tidal--prefetch-queue)
      (setq cape-tidal--prefetch-queue
            (delete candidate cape-tidal--prefetch-queue))
      (setq cape-tidal--prefetch-demand
            (append cape-tidal--prefetch-demand
                    (list (substring-no-properties candidate)))))))

(defun cape-tidal--annotation-function (candidate)
  "Return the annotation for CANDIDATE.
The value only reads the cache and never queries GHCi from here (Company does
not
actually support asynchronous annotations and blocks in a `sleep-for' loop). 
But
for an unfetched candidate, record that its display was requested as a demand
hint and move it to the front of the prefetch queue.  Corfu calls annotation
only
for candidates in the visible range, so this attaches types starting from the
candidates on screen.  A redraw after fetching is not guaranteed — it appears
on
the next popup update."
  (let ((tidal-buf (cape-tidal--tidal-buffer)))
    (when tidal-buf
      (let* ((cache (buffer-local-value 'cape-tidal--type-cache tidal-buf))
             (cached (if cache
                         (gethash candidate cache 'cape-tidal--miss)
                       'cape-tidal--miss)))
        (cond
         ;; Append one trailing space.  The annotation is right-aligned, so
         ;; with
         ;; an italic face (many themes' `completions-annotations') the
         ;; overhang
         ;; of the last glyph is clipped at the popup's right edge.  Make the
         ;; last
         ;; visible character a space to escape it.  The cache is kept without
         ;; the
         ;; space (display concern only).
         ((stringp cached) (concat cached " "))
         ;; Unfetched: emit only the demand hint and return nil (the
         ;; negative-cache
         ;; sentinel does nothing).
         ((eq cached 'cape-tidal--miss)
          (cape-tidal--prioritize-prefetch candidate tidal-buf)
          nil)
         (t nil))))))

(defun cape-tidal--company-kind (_candidate)
  "Return the kind symbol for CANDIDATE.
Used by Corfu/Company to display an icon."
  'function)

(defun cape-tidal--fetch-candidates-sync (prefix)
  "Fetch the completion candidates for PREFIX synchronously (with a timeout).
The value is the response plist from `cape-tidal--parse-complete-output'.  It
returns (:ok nil) even when the wait comes up empty, so it can be told apart
from
a successful 0 results.  After fetching, start the background prefetch of type
information.

It waits synchronously, but the upper bound is `cape-tidal-sync-timeout'.  Past
it, give up and return (:ok nil).  A response that arrives after giving up is
discarded and internal state is folded down, so it is safe."
  (let ((result (list :ok nil))
        (done nil)
        (abandoned nil))
    (cape-tidal--get-candidates
     prefix
     (lambda (response)
       (unless abandoned
         (setq result response)
         (setq done t)
         ;; Start fetching type information in the background (only on
         ;; success).
         (when (plist-get response :ok)
           (cape-tidal--fetch-types-async (plist-get response :candidates))))))
    ;; Wait for the result.  Specify the process so we do not pull in other
    ;; processes' output handling (timers still run during this wait).  The
    ;; upper
    ;; bound of the wait is `cape-tidal-sync-timeout' — distinct from the state
    ;; machine's `cape-tidal-timeout', it is the limit on how long the user is
    ;; kept waiting.  It used to wait up to `2 * cape-tidal-timeout + 1' so as
    ;; not
    ;; to cut short the legitimate path (pending wait + post-send deadline),
    ;; but
    ;; since the cleanup below (`abandoned') keeps state from breaking when we
    ;; give up early, it was shortened to favor the user's experience.
    (let ((deadline (+ (float-time) cape-tidal-sync-timeout))
          (waiting t))
      ;; Separate `done' (a response was received) from `waiting' (there is a
      ;; reason to keep waiting).  Setting `done' when we stop waiting because
      ;; the
      ;; process vanished would skip the cleanup below, and a callback that
      ;; arrives late would slip through.
      (while (and (not done) waiting (< (float-time) deadline))
        (let* ((buf (cape-tidal--tidal-buffer))
               (proc (and buf (cape-tidal--live-process buf))))
          (if proc
              (accept-process-output proc 0.05)
            ;; If the process vanished, nothing is coming however long we wait.
            (setq waiting nil)))))
    (unless done
      ;; The backup deadline expired.  We give up on the result at this point,
      ;; so
      ;; seal off a late-arriving callback from starting a type prefetch or
      ;; rewriting the result.  Also fold the state machine down so a response
      ;; still on the wire is not misdelivered.
      (setq abandoned t)
      (let ((buf (cape-tidal--tidal-buffer)))
        (when buf
          (with-current-buffer buf
            (cape-tidal--drop-pending)
            (when (eq cape-tidal--state 'active)
              (cape-tidal--handle-timeout))))))
    result))

(defun cape-tidal--separator-p (input)
  "Return non-nil if INPUT contains a completion-style separator character.
orderless splits its search words on whitespace, so passing input containing a
separator straight to GHCi (as in `:complete repl 100 \"d 1\"') reliably yields
0 results."
  (or (string-match-p " " input)
      (let ((sep (and (bound-and-true-p corfu-mode)
                      (bound-and-true-p corfu-separator))))
        (and sep (string-match-p (regexp-quote (string sep)) input)))))

(defun cape-tidal--table-input (beg end buffer)
  "Return the real input between the markers BEG..END.  nil if they became invalid."
  (and (buffer-live-p buffer)
       (eq (marker-buffer beg) buffer)
       (eq (marker-buffer end) buffer)
       (<= (marker-position beg) (marker-position end))
       (with-current-buffer buffer
         ;; If the markers are outside the current narrowing,
         ;; `buffer-substring-no-properties' throws args-out-of-range.
         (save-restriction
           (widen)
           (buffer-substring-no-properties beg end)))))

(defun cape-tidal--make-table (beg end)
  "Build the completion table for one capf session over BEG..END.

The prefix passed to GHCi is taken fresh each time from the **real buffer text
between the markers**, not from the table's STRING argument.  A non-prefix
style
like orderless filters on its own and passes an empty string as STRING, so
using
STRING would send `:complete repl 100 \"\"' and get only the first N of all
candidates (candidates starting with a lowercase letter, like `d1', buried
under
the limit and lost).  STRING is passed only to `complete-with-action', leaving
filtering to the completion style.

If the markers become invalid, point at a different buffer, or get out of
order,
finish with no candidates.  Do not fall back to STRING — it drops to the empty
string and reintroduces the same bug.

Whether to refetch is decided from the header's printed/total.  If printed =
total the fetched set is complete (exhaustive), so as long as the input is an
extension of it, local filtering is enough.  Do not use the response length as
a
substitute — it would mistake a communication failure's nil for `a settled 0
results' and stop refetching thereafter."
  (let ((beg (copy-marker beg))
        (end (copy-marker end t))
        (buffer (current-buffer))
        (cached-input nil)
        (cached-candidates nil)
        (attempted nil)
        (exhaustive nil)
        (have-result nil)
        (demand-input 'cape-tidal--none))
    (lambda (string pred action)
      (if (or (eq (car-safe action) 'boundaries) (eq action 'metadata))
          nil
        (let ((input (cape-tidal--table-input beg end buffer)))
          ;; When the input changes the candidates on screen change too.  Drop
          ;; the demand hint for candidates that were visible under the
          ;; previous
          ;; input.  This must happen here, since it also runs when candidates
          ;; were merely narrowed by local filtering (no re-hit to GHCi) —
          ;; otherwise a candidate visible at `se' stays at the front of the
          ;; demand FIFO after narrowing to `setcps', and the target type
          ;; appears
          ;; late, waiting its turn for the budget.
          (when (and input (not (equal input demand-input)))
            (setq demand-input input)
            (let ((tidal-buf (cape-tidal--tidal-buffer)))
              (when tidal-buf
                (cape-tidal--demand-reset tidal-buf))))
          (if (null input)
              ;; The markers became invalid (buffer killed, etc.).  The fetched
              ;; candidates no longer map to any input, so drop them and return
              ;; no
              ;; candidates.
              (progn
                (setq cached-candidates nil
                      cached-input nil
                      attempted nil
                      exhaustive nil
                      have-result nil)
                (complete-with-action action nil string pred))
            (unless (or
                     ;; Do not pass empty input to GHCi (it only returns the
                     ;; first N of all candidates).
                     (string-empty-p input)
                     ;; Consecutive try/all/test calls for the same input.
                     (equal input attempted)
                     ;; Do not send input containing a separator.  Get by on
                     ;; the
                     ;; existing candidates if any, otherwise no candidates
                     ;; (sending would reliably yield 0 anyway).
                     (cape-tidal--separator-p input)
                     ;; Fully fetched, and if the input is an extension of it,
                     ;; local filtering is enough.
                     (and have-result exhaustive cached-input
                          (string-prefix-p cached-input input)))
              (setq attempted input)
              (let ((res (cape-tidal--fetch-candidates-sync input)))
                (if (plist-get res :ok)
                    (setq cached-candidates (plist-get res :candidates)
                          cached-input input
                          exhaustive (= (plist-get res :printed)
                                        (plist-get res :total))
                          have-result t)
                  ;; Failure.  Do not drop the last successful candidates (keep
                  ;; them for local filtering).  Always drop exhaustive so a
                  ;; changed input can retry.
                  (setq exhaustive nil))))
            (complete-with-action action cached-candidates string pred)))))))

;;;###autoload
(defun cape-tidal ()
  "completion-at-point function for TidalCycles."
  (let ((tidal-buf (and (derived-mode-p 'tidal-mode)
                        (cape-tidal--tidal-buffer))))
    (when (and tidal-buf
               (cape-tidal--live-process tidal-buf)
               (not (cape-tidal--in-string-or-comment-p)))
      ;; Confirm and repair the setup (filter, advice, sentinel, restart
      ;; detection).
      (cape-tidal--ensure-filter-installed)
      (let ((bounds (cape-tidal--grab-symbol)))
        (when bounds
          (list (car bounds)
                (cdr bounds)
                (cape-tidal--make-table (car bounds) (cdr bounds))
                :exclusive 'no
                :annotation-function #'cape-tidal--annotation-function
                :company-kind #'cape-tidal--company-kind))))))

;;; Internal setup

(defun cape-tidal--sentinel (proc event)
  "A composite sentinel that resets all state when the process ends.
PROC and EVENT are the usual sentinel arguments.  Afterwards, always pass them
on
to the original sentinel that was saved (or Emacs's default handling if none)."
  (unwind-protect
      (when (memq (process-status proc) '(exit signal closed failed))
        (let ((buf (process-buffer proc)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              ;; Do nothing unless it is the current process that died.  After
              ;; GHCi is swapped, an old process's sentinel firing late would
              ;; take
              ;; the new process's state and type cache down with it.
              (when (eq proc cape-tidal--process)
                (cape-tidal--hard-reset)
                ;; Drop the cache too so old type annotations are not shown
                ;; after a restart.
                (setq cape-tidal--type-cache nil)
                (setq cape-tidal--process nil))))))
    (let ((prev (process-get proc 'cape-tidal--prev-sentinel)))
      (cond
       (prev (funcall prev proc event))
       ;; Reproduce the default sentinel handling (inserting the end message
       ;; into
       ;; the buffer, etc.).  It is confirmed that neither comint nor tidal.el
       ;; sets a sentinel (2026-07-17).
       ((fboundp 'internal-default-process-sentinel)
        (internal-default-process-sentinel proc event))))))

(defun cape-tidal--install-sentinel (proc)
  "Install the composite sentinel on PROC (once only).
`set-process-sentinel' overwrites, so save the original sentinel in a process
property and call it on from `cape-tidal--sentinel'."
  (unless (eq (process-sentinel proc) #'cape-tidal--sentinel)
    (process-put proc 'cape-tidal--prev-sentinel (process-sentinel proc))
    (set-process-sentinel proc #'cape-tidal--sentinel)))

(defun cape-tidal--restore-sentinel (proc)
  "Restore PROC's sentinel that we installed.
Do not touch it unless we installed it (another package may have swapped it in
later)."
  (when (and proc (eq (process-sentinel proc) #'cape-tidal--sentinel))
    (set-process-sentinel proc (process-get proc 'cape-tidal--prev-sentinel))
    (process-put proc 'cape-tidal--prev-sentinel nil)))

(defun cape-tidal--buffer-killed ()
  "Cleanup when the tidal buffer is killed.  Release any remaining timers."
  (cape-tidal--cancel-timeout)
  (cape-tidal--prefetch-cancel))

(defun cape-tidal--install-advice ()
  "Install the foreign-send-detection advice on `tidal-send-string' (once only)."
  (when (and (fboundp 'tidal-send-string)
             (not (advice-member-p #'cape-tidal--before-tidal-send
                                   'tidal-send-string)))
    (advice-add 'tidal-send-string :before #'cape-tidal--before-tidal-send)))

(defun cape-tidal--remove-advice ()
  "Remove the advice installed by `cape-tidal--install-advice'."
  (when (fboundp 'tidal-send-string)
    (advice-remove 'tidal-send-string #'cape-tidal--before-tidal-send)))

(defun cape-tidal--ensure-filter-installed ()
  "Confirm and repair the tidal buffer's setup.
In addition to installing the filter, hooks, advice, and sentinel, when a GHCi
restart (a process swap in the same buffer) is detected, drop the state and
type
cache and reinstall.  Safe to call any number of times."
  (let ((tidal-buf (cape-tidal--tidal-buffer)))
    (when tidal-buf
      (with-current-buffer tidal-buf
        (unless cape-tidal--filter-installed
          (add-hook 'comint-preoutput-filter-functions
                    #'cape-tidal--preoutput-filter nil t)
          ;; Count manual input (comint-send-input) as a foreign send too.
          (add-hook 'comint-input-filter-functions
                    #'cape-tidal--comint-input-noticed nil t)
          (add-hook 'kill-buffer-hook #'cape-tidal--buffer-killed nil t)
          (cape-tidal--install-advice)
          (setq cape-tidal--filter-installed t))
        (unless cape-tidal--type-cache
          (setq cape-tidal--type-cache (make-hash-table :test 'equal)))
        ;; Detect a process swap (GHCi restart).
        (let ((proc (get-buffer-process tidal-buf)))
          (when (and proc (not (eq proc cape-tidal--process)))
            (when cape-tidal--process
              ;; State and cache for the old process cannot be trusted.
              (cape-tidal--hard-reset)
              (setq cape-tidal--type-cache (make-hash-table :test 'equal))
              ;; If the old process is still alive, do not leave the composite
              ;; sentinel on it.  Leaving it would become a void-function the
              ;; moment that process dies after unload (teardown only looks at
              ;; the
              ;; current process).
              (cape-tidal--restore-sentinel cape-tidal--process))
            (setq cape-tidal--process proc)
            (cape-tidal--install-sentinel proc)))))))

(defun cape-tidal--teardown-buffer ()
  "Remove the hooks, timers, and sentinel installed in the current buffer."
  (when cape-tidal--filter-installed
    (remove-hook 'comint-preoutput-filter-functions
                 #'cape-tidal--preoutput-filter t)
    (remove-hook 'comint-input-filter-functions
                 #'cape-tidal--comint-input-noticed t)
    (remove-hook 'kill-buffer-hook #'cape-tidal--buffer-killed t)
    (cape-tidal--cancel-timeout)
    (cape-tidal--prefetch-cancel)
    ;; Restore the process we own, not `get-buffer-process'.
    (cape-tidal--restore-sentinel cape-tidal--process)
    (setq cape-tidal--filter-installed nil)
    (setq cape-tidal--process nil)))

(defun cape-tidal-unload-function ()
  "Cleanup on `unload-feature'.  Return nil to continue with standard handling.
Removing the advice alone is not enough.  If the buffer-local preoutput filter,
input hook, kill hook, timeout timer, and process sentinel are left in place
when
the function definitions vanish, the next process output becomes a
`void-function'."
  (cape-tidal--remove-advice)
  (when cape-tidal--refresh-timer
    (cancel-timer cape-tidal--refresh-timer)
    (setq cape-tidal--refresh-timer nil))
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cape-tidal--teardown-buffer))))
  nil)

(provide 'cape-tidal)
;;; cape-tidal.el ends here
