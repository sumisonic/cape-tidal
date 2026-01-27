;;; cape-tidal-test.el --- Tests for cape-tidal -*- lexical-binding: t -*-

;; Copyright (C) 2026 sumisonic

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT tests for cape-tidal.el.  They target pure logic that runs without
;; GHCi (the output parser and the request state machine).  How to run:
;;
;;   command emacs -Q --batch -L . -l cape-tidal.el \
;;     -l test/cape-tidal-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'comint)
(require 'cape-tidal)

(defconst cape-tidal-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file.  Used to locate fake-ghci.sh.")

;; The (defvar tidal-buffer) in cape-tidal.el is special only inside the file
;; that declares it, so declare it here too to make let bind it dynamically.
(defvar tidal-buffer)
(declare-function tidal-send-string "tidal" (s))
(defvar corfu-mode)
(defvar corfu--candidates)
(defvar corfu--base)

;;; Test helpers

(defmacro cape-tidal-test--with-tidal-buffer (&rest body)
  "Run BODY with a test tidal buffer as the current buffer.
Bind `tidal-buffer' dynamically; on exit stop the timer and kill the buffer."
  (declare (indent 0) (debug t))
  `(let ((tidal-buffer "*cape-tidal-test*"))
     (unwind-protect
         (with-current-buffer (get-buffer-create tidal-buffer)
           (cape-tidal--reset-state)
           (setq cape-tidal--pending nil)
           (setq cape-tidal--request-id 0)
           (setq cape-tidal--foreign-pending 0)
           (setq cape-tidal--last-foreign-time nil)
           (setq cape-tidal--prefetch-generation 0)
           (setq cape-tidal--type-cache nil)
           ,@body)
       (let ((buf (get-buffer "*cape-tidal-test*")))
         (when buf
           (with-current-buffer buf
             (cape-tidal--cancel-timeout))
           (kill-buffer buf))))))

(defmacro cape-tidal-test--with-fake-process (sent-var &rest body)
  "Run BODY with sending stubbed out.
The strings sent are pushed onto SENT-VAR (a list) newest first.
`cape-tidal--live-process' always returns non-nil."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,sent-var nil))
     (cl-letf (((symbol-function 'cape-tidal--send-string)
                (lambda (s) (push s ,sent-var)))
               ((symbol-function 'cape-tidal--live-process)
                (lambda (_buf) t)))
       ,@body)))

(defconst cape-tidal-test--complete-output
  "3 3 \"\"\n\"stut\"\n\"stutter\"\n\"stutWith\"\ntidal> "
  "Typical `:complete repl' output (a meta line, 3 candidates, a prompt).")

(defun cape-tidal-test--resp (candidates &optional total)
  "Build a success response plist containing CANDIDATES.
With TOTAL omitted, assume nothing was dropped."
  (list :ok t
        :printed (length candidates)
        :total (or total (length candidates))
        :common-prefix ""
        :candidates candidates))

(defun cape-tidal-test--type (str)
  "Insert STR at the end of the current buffer (like the user typing)."
  (goto-char (point-max))
  (insert str))

;;; Output parser

(ert-deftest cape-tidal-test-parse-complete-basic ()
  "Candidates minus the header and prompt come back with printed/total."
  (let ((r (cape-tidal--parse-complete-output
            cape-tidal-test--complete-output)))
    (should (plist-get r :ok))
    (should (equal (plist-get r :candidates) '("stut" "stutter" "stutWith")))
    (should (= (plist-get r :printed) 3))
    (should (= (plist-get r :total) 3))))

(ert-deftest cape-tidal-test-parse-complete-empty ()
  "A legitimate 0 results is a success.  Do not confuse it with a fetch failure."
  (let ((r (cape-tidal--parse-complete-output "0 0 \"\"\ntidal> ")))
    (should (plist-get r :ok))
    (should (null (plist-get r :candidates)))))

(ert-deftest cape-tidal-test-parse-complete-failures ()
  "A fetch failure or broken response is :ok nil, told apart from a legit 0."
  ;; No response (timeout, process death).
  (should-not (plist-get (cape-tidal--parse-complete-output nil) :ok))
  ;; No header.
  (should-not (plist-get (cape-tidal--parse-complete-output "\"stut\"\n") :ok))
  ;; Declared count disagrees with the actual line count.
  (should-not (plist-get (cape-tidal--parse-complete-output
                          "3 3 \"\"\n\"a\"\n\"b\"\n")
                         :ok))
  ;; An unquoted candidate line.
  (should-not (plist-get (cape-tidal--parse-complete-output "1 1 \"\"\nstut\n")
                         :ok)))

(ert-deftest cape-tidal-test-parse-complete-decodes-escapes ()
  "Candidates are decoded as Haskell string literals.
`Data.List''s set-difference operator is 2 chars but arrives as a 6-char line."
  (let* ((line (concat "\"" (make-string 4 ?\\) "\""))
         (r (cape-tidal--parse-complete-output
             (concat "1 1 \"\"\n" line "\ntidal> "))))
    (should (plist-get r :ok))
    (should (equal (plist-get r :candidates) (list (make-string 2 ?\\))))))

(ert-deftest cape-tidal-test-parse-complete-common-prefix ()
  "The header's common-prefix is prepended to each candidate (GHC spec)."
  (let ((r (cape-tidal--parse-complete-output
            "2 2 \"import \"\n\"Foreign\"\n\"Foreign.C\"\ntidal> ")))
    (should (equal (plist-get r :candidates)
                   '("import Foreign" "import Foreign.C")))))

(ert-deftest cape-tidal-test-parse-complete-prompt-on-header-line ()
  "When the prompt shares the header line, strip it, do not drop the line."
  (let ((r (cape-tidal--parse-complete-output "tidal> 1 1 \"\"\n\"d1\"\ntidal> ")))
    (should (plist-get r :ok))
    (should (equal (plist-get r :candidates) '("d1")))))

(ert-deftest cape-tidal-test-parse-type-basic ()
  "Extract \":: TYPE\" from the type output."
  (should (equal (cape-tidal--parse-type-output
                  "stut :: Pattern a -> Pattern a\ntidal> ")
                 ":: Pattern a -> Pattern a")))

(ert-deftest cape-tidal-test-parse-type-multiline ()
  "A multi-line type is joined with spaces."
  (should (equal (cape-tidal--parse-type-output
                  "foo\n  :: (Num a)\n  => a -> a\ntidal> ")
                 ":: (Num a) => a -> a")))

(ert-deftest cape-tidal-test-parse-type-error-output ()
  "Error output (no ::) is nil."
  (should (null (cape-tidal--parse-type-output
                 "<interactive>:1:8: error: parse error\ntidal> "))))

;;; Prompt boundary

(ert-deftest cape-tidal-test-find-boundary ()
  "Boundary detection: none / at end / mid remainder / including the space after."
  (should (null (cape-tidal--find-boundary "still going")))
  (should (null (cape-tidal--find-boundary nil)))
  (let* ((str "resp\ntidal> ")
         (end (cape-tidal--find-boundary str)))
    (should (equal end (length str))))
  (let* ((str "resp\ntidal> after")
         (end (cape-tidal--find-boundary str)))
    (should (equal (substring str end) "after"))))

;;; Filter state machine

(ert-deftest cape-tidal-test-filter-idle-passthrough ()
  "When idle, do not touch the output."
  (cape-tidal-test--with-tidal-buffer
    (should (equal (cape-tidal--preoutput-filter "hello\ntidal> ")
                   "hello\ntidal> "))))

(ert-deftest cape-tidal-test-filter-active-chunked-response ()
  "Accumulate a chunked response and pass it to the callback at the boundary."
  (cape-tidal-test--with-tidal-buffer
    (let ((got 'unset))
      (setq cape-tidal--state 'active
            cape-tidal--callback (lambda (r) (setq got r))
            cape-tidal--deadline (+ (float-time) 5.0))
      (should (equal (cape-tidal--preoutput-filter "3 3 \"\"\n\"stut\"\n") ""))
      (should (eq got 'unset))
      (should (equal (cape-tidal--preoutput-filter "\"stutter\"\ntidal> ") ""))
      (should (equal got "3 3 \"\"\n\"stut\"\n\"stutter\"\ntidal> "))
      (should (null cape-tidal--state)))))

(ert-deftest cape-tidal-test-filter-active-remainder-passthrough ()
  "The remainder after the boundary goes back to comint."
  (cape-tidal-test--with-tidal-buffer
    (let ((got nil))
      (setq cape-tidal--state 'active
            cape-tidal--callback (lambda (r) (setq got r))
            cape-tidal--deadline (+ (float-time) 5.0))
      (should (equal (cape-tidal--preoutput-filter "resp\ntidal> user output")
                     "user output"))
      (should (equal got "resp\ntidal> "))
      (should (null cape-tidal--state)))))

(ert-deftest cape-tidal-test-filter-draining-discards ()
  "While draining, discard up to the boundary; no callback, only remainder."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--state 'draining
          cape-tidal--callback nil
          cape-tidal--deadline (+ (float-time) 5.0))
    (should (equal (cape-tidal--preoutput-filter "late resp\ntidal> rest")
                   "rest"))
    (should (null cape-tidal--state))))

(ert-deftest cape-tidal-test-filter-fail-open-on-expiry ()
  "On deadline, lift suppression and pass output through.  The callback gets nil."
  (cape-tidal-test--with-tidal-buffer
    (let ((got 'unset))
      (setq cape-tidal--state 'active
            cape-tidal--callback (lambda (r) (setq got r))
            cape-tidal--output "partial"
            cape-tidal--deadline (- (float-time) 1.0))
      (should (equal (cape-tidal--preoutput-filter "output") "output"))
      (should (null got))
      (should (null cape-tidal--state))
      ;; The fragment addressed to us (the accumulated part) is discarded.
      (should (null cape-tidal--output)))))

(ert-deftest cape-tidal-test-filter-fail-open-interrupted-returns-accumulated ()
  "A user-interrupt-driven fail open returns the accumulated part to comint too."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--state 'draining
          cape-tidal--interrupted t
          cape-tidal--output "user eval output\n"
          cape-tidal--deadline (- (float-time) 1.0))
    (should (equal (cape-tidal--preoutput-filter "more")
                   "user eval output\nmore"))
    (should (null cape-tidal--state))))

;;; Sending and queueing

(ert-deftest cape-tidal-test-send-request-transmits-when-idle ()
  "When idle, send immediately and become active."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--send-request ":complete repl 100 \"st\"" #'ignore t)
      (should (equal sent '(":complete repl 100 \"st\"")))
      (should (eq cape-tidal--state 'active)))))

(ert-deftest cape-tidal-test-send-request-queues-and-flushes ()
  "A completion request while busy is queued and sent after the boundary."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((r1-got 'unset))
        (cape-tidal--send-request "req1" (lambda (r) (setq r1-got r)) t)
        (cape-tidal--send-request "req2" #'ignore t)
        ;; req2 has not been sent yet.
        (should (equal sent '("req1")))
        (should cape-tidal--pending)
        ;; req1 response arrives -> req1 callback + req2 sent.
        (cape-tidal--preoutput-filter "resp1\ntidal> ")
        (should (equal r1-got "resp1\ntidal> "))
        (should (equal sent '("req2" "req1")))
        (should (eq cape-tidal--state 'active))
        (should (null cape-tidal--pending))))))

(ert-deftest cape-tidal-test-send-request-pending-latest-wins ()
  "The pending is one slot, latest wins.  The evicted callback gets nil."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((r2-got 'unset))
        (cape-tidal--send-request "req1" #'ignore t)
        (cape-tidal--send-request "req2" (lambda (r) (setq r2-got r)) t)
        (cape-tidal--send-request "req3" #'ignore t)
        (should (null r2-got))
        (should (equal (nth 0 cape-tidal--pending) "req3"))))))

(ert-deftest cape-tidal-test-send-request-no-queue-gets-nil-when-busy ()
  "A non-queue call (type prefetch) gets nil immediately when busy, pending untouched."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((got 'unset))
        (cape-tidal--send-request "req1" #'ignore t)
        (cape-tidal--send-request ":type (stut)" (lambda (r) (setq got r)))
        (should (null got))
        (should (null cape-tidal--pending))
        (should (equal sent '("req1")))))))

(ert-deftest cape-tidal-test-send-request-nil-without-process ()
  "With no process, the callback gets nil immediately (no error)."
  (cape-tidal-test--with-tidal-buffer
    (cl-letf (((symbol-function 'cape-tidal--live-process)
               (lambda (_buf) nil)))
      (let ((got 'unset))
        (cape-tidal--send-request "req" (lambda (r) (setq got r)) t)
        (should (null got))
        (should (null cape-tidal--state))))))

;;; Timeout and timers

(ert-deftest cape-tidal-test-timeout-transitions-to-draining ()
  "On timeout, active -> draining.  The callback gets nil."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((got 'unset))
        (cape-tidal--send-request "req" (lambda (r) (setq got r)) t)
        (cape-tidal--handle-timeout)
        (should (null got))
        (should (eq cape-tidal--state 'draining))
        ;; The late response is discarded and the remainder returns.
        (should (equal (cape-tidal--preoutput-filter "late\ntidal> rest") "rest"))
        (should (null cape-tidal--state))))))

(ert-deftest cape-tidal-test-timeout-dead-process-resets-fully ()
  "If the process is dead, clear everything without draining and drop the pending."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--send-request "req1" #'ignore t)
      (cape-tidal--send-request "req2" #'ignore t))
    ;; Here the process is dead (no live-process stub = nil).
    (cape-tidal--handle-timeout)
    (should (null cape-tidal--state))
    (should (null cape-tidal--pending))))

(ert-deftest cape-tidal-test-stale-timer-is-ignored ()
  "A timer firing with an old ID does not affect the current request."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--send-request "req" #'ignore t)
      (let ((buf (current-buffer)))
        (cape-tidal--timer-fired buf (1- cape-tidal--request-id))
        (should (eq cape-tidal--state 'active))))))

;;; Coexisting with playing (foreign sends)

(ert-deftest cape-tidal-test-foreign-send-interrupts-active ()
  "A foreign send makes an active completion lose immediately, demoted to a short drain."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((got 'unset))
        (cape-tidal--send-request "req" (lambda (r) (setq got r)) t)
        (should (eq cape-tidal--state 'active))
        (cape-tidal--note-foreign-send)
        ;; The completion callback gets nil immediately.
        (should (null got))
        (should (eq cape-tidal--state 'draining))
        (should cape-tidal--interrupted)
        ;; The late response addressed to us is discarded.
        (should (equal (cape-tidal--preoutput-filter "late\ntidal> ") ""))
        ;; The following playing response passes straight through.
        (should (equal (cape-tidal--preoutput-filter "played\ntidal> ")
                       "played\ntidal> "))
        (should (zerop cape-tidal--foreign-pending))))))

(ert-deftest cape-tidal-test-foreign-gate-queues-completion ()
  "Do not send completion while waiting on a foreign response; send once it clears."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--note-foreign-send)
      (cape-tidal--send-request "req" #'ignore t)
      ;; Not sent yet (waiting in pending).
      (should (null sent))
      (should cape-tidal--pending)
      ;; The playing response finishes -> the pending is sent.
      (should (equal (cape-tidal--preoutput-filter "eval output\ntidal> ")
                     "eval output\ntidal> "))
      (should (equal sent '("req")))
      (should (eq cape-tidal--state 'active)))))

(ert-deftest cape-tidal-test-foreign-counter-staleness-recovery ()
  "Even if a prompt is missed, the counter returns to 0 with elapsed time."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 3)
    (setq cape-tidal--last-foreign-time (- (float-time) 10.0))
    (should (cape-tidal--clear-to-send-p))
    (should (zerop cape-tidal--foreign-pending))))

(ert-deftest cape-tidal-test-advice-detects-foreign-send ()
  "The advice detects only foreign sends and ignores our own send during own-send."
  (cape-tidal-test--with-tidal-buffer
    (unwind-protect
        (progn
          (defalias 'tidal-send-string (lambda (_s) nil))
          (cape-tidal--install-advice)
          (tidal-send-string "d1 $ sound \"bd\"")
          (should (= cape-tidal--foreign-pending 1))
          (let ((cape-tidal--own-send t))
            (tidal-send-string ":complete repl 100 \"st\""))
          (should (= cape-tidal--foreign-pending 1)))
      (cape-tidal--remove-advice)
      (fmakunbound 'tidal-send-string))))

;;; Type prefetch scheduler

(defun cape-tidal-test--prefetch-step ()
  "Run the scheduled prefetch step now, without waiting for the timer."
  (when cape-tidal--prefetch-timer
    (cancel-timer cape-tidal--prefetch-timer)
    (setq cape-tidal--prefetch-timer nil))
  (cape-tidal--prefetch-step (current-buffer) cape-tidal--prefetch-generation))

(ert-deftest cape-tidal-test-prefetch-sends-one-per-step-in-order ()
  "One step sends exactly one, in queue order.  No synchronous send at startup."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("stut" "stutter" "stutWith"))
      ;; Only scheduled; nothing sent yet.
      (should cape-tidal--prefetch-timer)
      (should (null sent))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (stut)")))
      ;; Response -> the next is rescheduled (not sent immediately).
      (cape-tidal--preoutput-filter "stut :: A\ntidal> ")
      (should (equal sent '(":type (stut)")))
      (should cape-tidal--prefetch-timer)
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (stutter)" ":type (stut)"))))))

(ert-deftest cape-tidal-test-prefetch-respects-budget ()
  "Once the budget is spent, do not send even if the queue has more."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((cape-tidal-prefetch-limit 1))
        (cape-tidal--fetch-types-async '("stut" "stutter" "stutWith"))
        (cape-tidal-test--prefetch-step)
        (should (equal sent '(":type (stut)")))
        (cape-tidal--preoutput-filter "stut :: A\ntidal> ")
        (cape-tidal-test--prefetch-step)
        (should (equal sent '(":type (stut)")))
        (should (null cape-tidal--prefetch-timer))))))

(ert-deftest cape-tidal-test-prefetch-pauses-and-resumes-when-busy ()
  "While busy, reschedule instead of sending (no vanishing); resume once quiet."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      ;; A completion request is active.
      (cape-tidal--send-request "completion" #'ignore t)
      (cape-tidal--fetch-types-async '("a" "b"))
      (cape-tidal-test--prefetch-step)
      ;; Not sent, but the queue remains and it is rescheduled.
      (should (equal sent '("completion")))
      (should (equal cape-tidal--prefetch-queue '("a" "b")))
      (should cape-tidal--prefetch-timer)
      ;; The completion clears -> resumes on the next step.
      (cape-tidal--preoutput-filter "1 1 \"\"\n\"x\"\ntidal> ")
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (a)" "completion"))))))

(ert-deftest cape-tidal-test-prefetch-pauses-on-foreign-send ()
  "A foreign send (playing) pauses it; it resumes once that response clears.
The generation used to advance on a foreign send, making the prefetch vanish."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("a" "b"))
      (let ((gen cape-tidal--prefetch-generation))
        ;; The user evaluates a pattern.
        (cape-tidal--note-foreign-send)
        ;; The generation is unchanged (a pause, not an invalidate).
        (should (eql gen cape-tidal--prefetch-generation))
        (cape-tidal-test--prefetch-step)
        (should (null sent))
        (should (equal cape-tidal--prefetch-queue '("a" "b")))
        ;; The evaluation response (prompt) arrives and clear-to-send holds.
        (cape-tidal--preoutput-filter "played\ntidal> ")
        ;; Right after, the quiet delay blocks sending (a separate test covers
        ;; that); here we push the wait into the past.
        (setq cape-tidal--last-foreign-done (- (float-time) 10))
        (cape-tidal-test--prefetch-step)
        (should (equal sent '(":type (a)")))))))

(ert-deftest cape-tidal-test-prefetch-quiet-period-after-foreign-done ()
  "Do not send for `cape-tidal-prefetch-delay' seconds after a foreign send finishes
(quiet delay).  Even when an evaluation arrives and clears after the timer was
scheduled, measure again from the finish time."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("a"))
      (cape-tidal--note-foreign-send)
      (cape-tidal--preoutput-filter "played\ntidal> ")
      ;; Right after finishing: clear-to-send, but within the quiet delay.
      (should (cape-tidal--clear-to-send-p))
      (cape-tidal-test--prefetch-step)
      (should (null sent))
      (should cape-tidal--prefetch-timer)
      ;; Send once the quiet delay is over.
      (setq cape-tidal--last-foreign-done
            (- (float-time) cape-tidal-prefetch-delay 0.01))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (a)"))))))

(ert-deftest cape-tidal-test-prefetch-requeues-interrupted-candidate ()
  "A candidate whose type was not obtained because playing cut in after the send but
before the response is put back at the front and retried.  It used to be already
popped, so it never returned to the queue and that candidate was lost forever."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("a" "b"))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (a)")))
      ;; The user evaluates before the response -> the active :type loses and
      ;; its callback gets nil.
      (cape-tidal--note-foreign-send)
      (should (null (gethash "a" cape-tidal--type-cache)))
      ;; "a" is back at the front of the demand FIFO.
      (should (equal cape-tidal--prefetch-demand '("a")))
      (should (equal cape-tidal--prefetch-queue '("b")))
      ;; GHCi returns two responses: the interrupted :type's response (discarded
      ;; by draining) and the evaluation output.  The latter's prompt is what
      ;; counts the foreign send as finished.
      (cape-tidal--preoutput-filter "a :: A\ntidal> ")
      (should (null cape-tidal--state))
      (cape-tidal--preoutput-filter "played\ntidal> ")
      (should (cape-tidal--clear-to-send-p))
      ;; Once the quiet delay is over, retry starting from "a".
      (setq cape-tidal--last-foreign-done (- (float-time) 10))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (a)" ":type (a)"))))))

(ert-deftest cape-tidal-test-prefetch-new-session-replaces-queue ()
  "When a new completion session starts, the old queue becomes invalid."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("a" "b"))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (a)")))
      ;; Session 2.
      (cape-tidal--fetch-types-async '("x" "y"))
      (should (equal cape-tidal--prefetch-queue '("x" "y")))
      ;; Session 1's response -> the old generation's callback does not schedule.
      (let ((timer-before cape-tidal--prefetch-timer))
        (cape-tidal--preoutput-filter "a :: A\ntidal> ")
        (should (eq cape-tidal--prefetch-timer timer-before)))
      ;; Session 2 starts from "x", not "b".
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (x)" ":type (a)"))))))

(ert-deftest cape-tidal-test-prefetch-skips-known-without-budget ()
  "Already-fetched and negative-cached candidates are skipped without spending budget."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (setq cape-tidal--type-cache (make-hash-table :test 'equal))
      (puthash "a" ":: A" cape-tidal--type-cache)
      (puthash "b" 'cape-tidal--none cape-tidal--type-cache)
      (let ((cape-tidal-prefetch-limit 1))
        (cape-tidal--fetch-types-async '("a" "b" "c"))
        (cape-tidal-test--prefetch-step)
        (should (equal sent '(":type (c)")))))))

(ert-deftest cape-tidal-test-annotation-has-trailing-space ()
  "The display annotation has a trailing space (guards against right-aligned italic
overhang clipping).  The cache side is kept without the space."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--type-cache (make-hash-table :test 'equal))
    (puthash "d1" ":: ControlPattern -> IO ()" cape-tidal--type-cache)
    (should (equal (cape-tidal--annotation-function "d1")
                   ":: ControlPattern -> IO () "))
    ;; The cache itself is not dirtied.
    (should (equal (gethash "d1" cape-tidal--type-cache)
                   ":: ControlPattern -> IO ()"))))

(ert-deftest cape-tidal-test-annotation-prioritizes-visible-candidate ()
  "An unfetched candidate whose annotation is called moves to the front (demand hint)."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("a" "b" "c" "d"))
      ;; "c" was shown on screen.
      (should (null (cape-tidal--annotation-function "c")))
      (should (equal cape-tidal--prefetch-demand '("c")))
      (should (equal cape-tidal--prefetch-queue '("a" "b" "d")))
      ;; annotation sends nothing to GHCi and adds no timer.
      (should (null sent))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (c)"))))))

(ert-deftest cape-tidal-test-demand-reset-returns-to-queue ()
  "Resetting demand returns the not-yet-fetched candidates to the queue at low
priority rather than dropping them.  prioritize removes a candidate from the queue
as it moves it to demand, so simply dropping demand would mean it is never fetched
again (the cause of a bug where a top-of-screen type disappears while narrowing)."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      (cape-tidal--fetch-types-async '("a" "b" "c" "d"))
      ;; "b" and "c" are shown and move to demand.
      (cape-tidal--annotation-function "b")
      (cape-tidal--annotation-function "c")
      (should (equal cape-tidal--prefetch-demand '("b" "c")))
      (should (equal cape-tidal--prefetch-queue '("a" "d")))
      ;; The input changes and demand is reset -> b and c are not dropped but
      ;; returned to the tail of the queue.
      (cape-tidal--demand-reset (current-buffer))
      (should (null cape-tidal--prefetch-demand))
      (should (equal cape-tidal--prefetch-queue '("a" "d" "b" "c")))
      ;; No candidate is lost.
      (should (= (length cape-tidal--prefetch-queue) 4)))))

(ert-deftest cape-tidal-test-annotation-demand-keeps-display-order ()
  "Multiple visible candidates are fetched in the order their annotation was
requested (= display order).  Front insertion would fetch the bottom-of-screen
one first.  With a budget of 1, the first shown candidate is the one fetched."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((cape-tidal-prefetch-limit 1))
        (cape-tidal--fetch-types-async '("x" "a" "b" "c"))
        ;; corfu annotates the visible range top to bottom.
        (cape-tidal--annotation-function "a")
        (cape-tidal--annotation-function "b")
        (cape-tidal--annotation-function "c")
        (should (equal cape-tidal--prefetch-demand '("a" "b" "c")))
        ;; Requesting the same candidate again does not duplicate it.
        (cape-tidal--annotation-function "b")
        (should (equal cape-tidal--prefetch-demand '("a" "b" "c")))
        (cape-tidal-test--prefetch-step)
        (should (equal sent '(":type (a)")))))))

(ert-deftest cape-tidal-test-annotation-handles-propertized-candidate ()
  "Cache lookup and queue moves work even for a candidate with text properties from corfu."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (setq cape-tidal--type-cache (make-hash-table :test 'equal))
      (puthash "b" ":: B" cape-tidal--type-cache)
      (cape-tidal--fetch-types-async '("a" "b"))
      (let ((a (propertize "a" 'face 'bold))
            (b (propertize "b" 'face 'bold)))
        (should (equal (cape-tidal--annotation-function b) ":: B "))
        (should (null (cape-tidal--annotation-function a)))
        ;; The demand FIFO holds the string with properties stripped.
        (should (equal cape-tidal--prefetch-demand '("a")))
        (should (null (text-properties-at 0 (car cape-tidal--prefetch-demand))))
        (cape-tidal-test--prefetch-step)
        (should (equal sent '(":type (a)")))))))

(ert-deftest cape-tidal-test-prefetch-defers-while-input-pending ()
  "If the user is typing (`input-pending-p'), reschedule instead of sending."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (cape-tidal--fetch-types-async '("a"))
      (cl-letf (((symbol-function 'input-pending-p) (lambda (&optional _) t)))
        (cape-tidal-test--prefetch-step)
        (should (null sent))
        (should cape-tidal--prefetch-timer))
      (cape-tidal-test--prefetch-step)
      (should (equal sent '(":type (a)"))))))

(ert-deftest cape-tidal-test-sync-late-response-does-not-prefetch ()
  "A response that arrives after giving up at sync-timeout moves neither the result
nor the prefetch."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc (start-process "cape-tidal-test-late" (current-buffer) "cat"))
          (captured nil)
          (prefetched 'none))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (cl-letf (((symbol-function 'cape-tidal--get-candidates)
                 (lambda (_prefix callback)
                   (setq captured callback)
                   (with-current-buffer (cape-tidal--tidal-buffer)
                     (setq cape-tidal--state 'active)
                     (setq cape-tidal--callback callback)
                     (setq cape-tidal--deadline (+ (float-time) 100)))))
                ((symbol-function 'cape-tidal--fetch-types-async)
                 (lambda (c) (setq prefetched c))))
        (let* ((cape-tidal-sync-timeout 0.1)
               (cape-tidal-timeout 100)
               (res (cape-tidal--fetch-candidates-sync "d")))
          (should-not (plist-get res :ok))
          ;; A success response arrives late.
          (funcall captured (list :ok t :printed 1 :total 1
                                  :common-prefix "" :candidates '("d1")))
          (should (eq prefetched 'none))))
      (delete-process proc))))

(ert-deftest cape-tidal-test-annotation-ignores-known-and-foreign ()
  "Do not reorder for already-fetched, negative-cached, or out-of-queue candidates."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      (setq cape-tidal--type-cache (make-hash-table :test 'equal))
      (puthash "b" ":: B" cape-tidal--type-cache)
      (puthash "c" 'cape-tidal--none cape-tidal--type-cache)
      (cape-tidal--fetch-types-async '("a" "b" "c"))
      (should (equal (cape-tidal--annotation-function "b") ":: B "))
      (should (null (cape-tidal--annotation-function "c")))
      (should (null (cape-tidal--annotation-function "zzz")))
      (should (equal cape-tidal--prefetch-queue '("a" "b" "c"))))))

(ert-deftest cape-tidal-test-prefetch-cleared-on-hard-reset ()
  "hard-reset stops the timer and clears the queue and budget."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      (cape-tidal--fetch-types-async '("a" "b"))
      (should cape-tidal--prefetch-timer)
      (cape-tidal--hard-reset)
      (should (null cape-tidal--prefetch-timer))
      (should (null cape-tidal--prefetch-queue))
      (should (zerop cape-tidal--prefetch-budget)))))

(ert-deftest cape-tidal-test-sync-gives-up-at-sync-timeout ()
  "The synchronous wait is cut off at `cape-tidal-sync-timeout'."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc (start-process "cape-tidal-test-slow" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (cl-letf (((symbol-function 'cape-tidal--get-candidates)
                 (lambda (_prefix callback)
                   (with-current-buffer (cape-tidal--tidal-buffer)
                     (setq cape-tidal--state 'active)
                     (setq cape-tidal--callback callback)
                     (setq cape-tidal--deadline (+ (float-time) 100))))))
        (let* ((cape-tidal-sync-timeout 0.15)
               (cape-tidal-timeout 100)
               (t0 (float-time))
               (res (cape-tidal--fetch-candidates-sync "d"))
               (elapsed (- (float-time) t0)))
          (should-not (plist-get res :ok))
          ;; It returned at sync-timeout, not the state machine's timeout (100 s).
          (should (< elapsed 1.0))))
      (delete-process proc))))

(ert-deftest cape-tidal-test-foreign-block-counts-once ()
  "Count the three sends `:{' / body / `:}' as one foreign send.
Counting all three would not return to 0 on one prompt, and would stall until the
2-second forced recovery."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      (let ((cape-tidal--own-send nil))
        (cape-tidal--before-tidal-send ":{")
        (cape-tidal--before-tidal-send "d1 $ sound \"bd\"\n  # gain 1")
        (cape-tidal--before-tidal-send ":}")
        (should (= cape-tidal--foreign-pending 1))
        ;; GHCi returns one prompt for the whole block.
        (cape-tidal--note-idle-output "tidal> ")
        (should (zerop cape-tidal--foreign-pending))
        (should (cape-tidal--clear-to-send-p))
        ;; A single-line send is one each, as before.
        (cape-tidal--before-tidal-send "hush")
        (cape-tidal--before-tidal-send "hush")
        (should (= cape-tidal--foreign-pending 2))))))

(ert-deftest cape-tidal-test-idle-prompt-split-across-chunks ()
  "A prompt split across a chunk boundary (`tidal' + `> ') is also counted as a
finished foreign send.  comint splits output at arbitrary positions.  Missing it
stalls the counter, and both the completion pending and the prefetch stop until
the forced recovery (2 s)."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 1)
    (cape-tidal--note-idle-output "tidal")
    (should (= cape-tidal--foreign-pending 1))
    (cape-tidal--note-idle-output "> ")
    (should (zerop cape-tidal--foreign-pending))))

(ert-deftest cape-tidal-test-idle-prompt-after-prompt ()
  "When one prompt is immediately followed by another (`tidal> tidal> '), count both.
With no output from the evaluation, GHCi emits prompts back to back, not at a line
beginning."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 2)
    (cape-tidal--note-idle-output "tidal> ")
    (should (= cape-tidal--foreign-pending 1))
    (cape-tidal--note-idle-output "tidal> ")
    (should (zerop cape-tidal--foreign-pending))))

(ert-deftest cape-tidal-test-idle-prompt-not-double-counted ()
  "A counted prompt kept in the retained tail is not counted again in the next chunk."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 2)
    (cape-tidal--note-idle-output "tidal> ")
    (should (= cape-tidal--foreign-pending 1))
    ;; It does not decrement even when non-prompt output follows.
    (cape-tidal--note-idle-output "some output\n")
    (should (= cape-tidal--foreign-pending 1))
    (cape-tidal--note-idle-output "more")
    (should (= cape-tidal--foreign-pending 1))))

(ert-deftest cape-tidal-test-idle-two-prompts-in-one-chunk ()
  "`tidal> tidal> ' in one chunk still counts as two (rescanned within one call)."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 2)
    (cape-tidal--note-idle-output "tidal> tidal> ")
    (should (zerop cape-tidal--foreign-pending))))

(ert-deftest cape-tidal-test-idle-prompt-every-split-position ()
  "The prompt is counted no matter where it is split into two or three."
  (let ((prompt "out\ntidal> "))
    (dotimes (i (length prompt))
      (cape-tidal-test--with-tidal-buffer
        (setq cape-tidal--foreign-pending 1)
        (cape-tidal--note-idle-output (substring prompt 0 i))
        (cape-tidal--note-idle-output (substring prompt i))
        (should (zerop cape-tidal--foreign-pending))))
    (dotimes (i (length prompt))
      (dotimes (k (- (length prompt) i))
        (let ((j (+ i k)))
          (cape-tidal-test--with-tidal-buffer
            (setq cape-tidal--foreign-pending 1)
            (cape-tidal--note-idle-output (substring prompt 0 i))
            (cape-tidal--note-idle-output (substring prompt i j))
            (cape-tidal--note-idle-output (substring prompt j))
            (should (zerop cape-tidal--foreign-pending))))))))

(ert-deftest cape-tidal-test-idle-tail-midline-no-false-match ()
  "If a truncated fragment starts mid-line, do not let `^' match there.
A `tidal> ' appearing mid-line is not a prompt."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 1)
    ;; Arrange for the last 64 chars to start exactly with "tidal> ..." (not at a
    ;; line beginning).
    (cape-tidal--note-idle-output
     (concat "x tidal> " (make-string (- cape-tidal--idle-tail-max 7) ?b)))
    (should (= cape-tidal--foreign-pending 1))
    (should (string-prefix-p "tidal> " cape-tidal--idle-tail))
    (should-not cape-tidal--idle-tail-bol)
    ;; Even when the next chunk arrives, `^tidal>' does not hold at the fragment start.
    (cape-tidal--note-idle-output "c")
    (should (= cape-tidal--foreign-pending 1))
    ;; A real line-beginning prompt is counted.
    (cape-tidal--note-idle-output "\ntidal> ")
    (should (zerop cape-tidal--foreign-pending))))

(ert-deftest cape-tidal-test-foreign-block-via-manual-input ()
  "Manual input into comint (`comint-input-filter-functions') also counts a block as one."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      (cape-tidal--comint-input-noticed ":{")
      (cape-tidal--comint-input-noticed "d1 $ s \"bd\"")
      (cape-tidal--comint-input-noticed ":}")
      (should (= cape-tidal--foreign-pending 1)))))

(ert-deftest cape-tidal-test-foreign-send-count-edge-cases ()
  "A stray `:}' is an ordinary line, a `:{' inside a block is body, newlines count per line."
  (cape-tidal-test--with-tidal-buffer
    ;; A stray :} -> GHCi returns an error and a prompt.
    (should (= (cape-tidal--foreign-send-count ":}") 1))
    (should-not cape-tidal--in-foreign-block)
    ;; A :{ inside a block is body, not counted until it exits.
    (should (= (cape-tidal--foreign-send-count ":{") 1))
    (should (= (cape-tidal--foreign-send-count ":{") 0))
    (should (= (cape-tidal--foreign-send-count "body") 0))
    (should (= (cape-tidal--foreign-send-count ":}") 0))
    (should-not cape-tidal--in-foreign-block)
    ;; A single send with newlines.
    (should (= (cape-tidal--foreign-send-count "hush\nhush") 2))
    (should (= (cape-tidal--foreign-send-count ":{\nlet x = 1\n:}") 1))
    (should-not cape-tidal--in-foreign-block)
    ;; GHCi returns a prompt for a blank line too.
    (should (= (cape-tidal--foreign-send-count "") 1))))

(ert-deftest cape-tidal-test-forced-recovery-clears-block-state ()
  "The time-based forced recovery drops not just the counter but the block state and
fragment too.  Keeping only the block state would stop counting every later
foreign send forever."
  (cape-tidal-test--with-tidal-buffer
    (setq cape-tidal--foreign-pending 1)
    (setq cape-tidal--in-foreign-block t)
    (setq cape-tidal--idle-tail "tid")
    (setq cape-tidal--last-foreign-time (- (float-time) 100))
    (should (cape-tidal--clear-to-send-p))
    (should (zerop cape-tidal--foreign-pending))
    (should-not cape-tidal--in-foreign-block)
    (should (equal cape-tidal--idle-tail ""))
    ;; A send after recovery is counted normally.
    (should (= (cape-tidal--foreign-send-count "hush") 1))))

(ert-deftest cape-tidal-test-idle-tail-reset-after-boundary ()
  "A remainder starting right after our request's boundary prompt is counted at a line beginning."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      ;; Our request is active, and one foreign send happens meanwhile.
      (cape-tidal--send-request "req" #'ignore)
      (setq cape-tidal--foreign-pending 1)
      ;; Our response plus the foreign send's prompt arrive in one chunk.
      (cape-tidal--preoutput-filter "1 1 \"\"\n\"x\"\ntidal> tidal> ")
      (should (null cape-tidal--state))
      (should (zerop cape-tidal--foreign-pending)))))

(ert-deftest cape-tidal-test-completion-not-stalled-after-block-eval ()
  "Even right after a block evaluation, the completion pending goes through once one prompt returns."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (let ((cape-tidal--own-send nil))
        (cape-tidal--before-tidal-send ":{")
        (cape-tidal--before-tidal-send "d1 $ s \"bd\"")
        (cape-tidal--before-tidal-send ":}")
        ;; Completion right after evaluation -> pending, since we wait on the foreign send.
        (cape-tidal--send-request "completion" #'ignore t)
        (should cape-tidal--pending)
        (should (null sent))
        ;; One evaluation prompt returns -> the pending is sent.
        (cape-tidal--preoutput-filter "tidal> ")
        (should (equal sent '("completion")))))))

(ert-deftest cape-tidal-test-type-arrival-schedules-popup-refresh ()
  "When a type is obtained, schedule a popup redraw; when not, do not."
  (cape-tidal-test--with-tidal-buffer
    (cape-tidal-test--with-fake-process sent
      (ignore sent)
      (let ((scheduled 0))
        (cl-letf (((symbol-function 'cape-tidal--schedule-popup-refresh)
                   (lambda () (setq scheduled (1+ scheduled)))))
          (cape-tidal--fetch-types-async '("a" "b"))
          (cape-tidal-test--prefetch-step)
          (cape-tidal--preoutput-filter "a :: A\ntidal> ")
          (should (= scheduled 1))
          ;; When no type was obtained (negative cache), do not schedule.
          (setq cape-tidal--last-foreign-done nil)
          (cape-tidal-test--prefetch-step)
          (cape-tidal--preoutput-filter "error: not in scope\ntidal> ")
          (should (= scheduled 1)))))))

(defmacro cape-tidal-test--with-fake-corfu-session (&rest body)
  "Run BODY in a state that mimics a corfu completion session.
`completion-in-region--data' has an integer first and a marker last, as in new
corfu.  `popup-calls' holds the number of `corfu--candidates-popup' calls."
  (declare (indent 0) (debug t))
  `(let ((buf (generate-new-buffer " *cape-tidal-corfu*"))
         (popup-calls 0))
     (unwind-protect
         (progn
           (with-current-buffer buf
             (insert "d1")
             (setq-local corfu-mode t))
           (set-window-buffer (selected-window) buf)
           (let ((completion-in-region-mode t)
                 (completion-in-region--data
                  (list 1 (with-current-buffer buf (copy-marker (point-max) t))
                        nil nil nil))
                 (corfu--candidates '("d1" "d10"))
                 (corfu--base ""))
             (cl-letf (((symbol-function 'corfu--candidates-popup)
                        (lambda (_pos) (setq popup-calls (1+ popup-calls))))
                       ((symbol-function 'posn-at-point)
                        (lambda (&rest _) 'fake-posn)))
               ,@body)))
       (when cape-tidal--refresh-timer
         (cancel-timer cape-tidal--refresh-timer)
         (setq cape-tidal--refresh-timer nil))
       (setq cape-tidal--refresh-retries 0)
       (let ((kill-buffer-query-functions nil))
         (kill-buffer buf)))))

(ert-deftest cape-tidal-test-popup-refresh-redraws-corfu-session ()
  "While corfu is showing, the redraw runs once even with the real data shape (an
integer first).  It used to assume the first was a marker, so it was always a
no-op on new corfu."
  (cape-tidal-test--with-fake-corfu-session
    (cape-tidal--refresh-popup)
    (should (= popup-calls 1))))

(ert-deftest cape-tidal-test-popup-refresh-from-other-buffer ()
  "It redraws from the selected window even when the timer's current buffer is not
the completion buffer."
  (cape-tidal-test--with-fake-corfu-session
    (with-temp-buffer
      (cape-tidal--refresh-popup))
    (should (= popup-calls 1))))

(ert-deftest cape-tidal-test-popup-refresh-does-not-touch-buffer ()
  "The redraw does not finish, insert, or end the completion (the buffer is unchanged)."
  (cape-tidal-test--with-fake-corfu-session
    (let ((before (with-current-buffer buf (buffer-string))))
      (cape-tidal--refresh-popup)
      (should (equal (with-current-buffer buf (buffer-string)) before))
      (should completion-in-region-mode))))

(ert-deftest cape-tidal-test-popup-refresh-retries-when-input-pending ()
  "If input is pending, do not redraw now but schedule a retry (dropping it once
hides the type until the next redraw)."
  (cape-tidal-test--with-fake-corfu-session
    (cl-letf (((symbol-function 'input-pending-p) (lambda (&optional _) t)))
      (cape-tidal--refresh-popup))
    (should (= popup-calls 0))
    (should cape-tidal--refresh-timer)
    (should (= cape-tidal--refresh-retries 1))))

(ert-deftest cape-tidal-test-popup-refresh-swallows-quit ()
  "A `while-no-input' quit during drawing does not leak out of the timer."
  (cape-tidal-test--with-fake-corfu-session
    (cl-letf (((symbol-function 'corfu--candidates-popup)
               (lambda (_pos) (signal 'quit nil))))
      (cape-tidal--refresh-popup))
    (should t)))

(ert-deftest cape-tidal-test-popup-refresh-noop-without-corfu ()
  "The redraw does nothing (and does not error) when corfu is absent or not completing."
  (let ((completion-in-region-mode nil)
        (cape-tidal--refresh-timer 'dummy))
    (cape-tidal--refresh-popup)
    (should (null cape-tidal--refresh-timer))))

(ert-deftest cape-tidal-test-popup-refresh-is-debounced ()
  "Even when types arrive back to back, the redraw is scheduled only once."
  (let ((cape-tidal--refresh-timer nil))
    (unwind-protect
        (progn
          (cape-tidal--schedule-popup-refresh)
          (let ((first cape-tidal--refresh-timer))
            (should first)
            (cape-tidal--schedule-popup-refresh)
            (should (eq cape-tidal--refresh-timer first))))
      (when cape-tidal--refresh-timer
        (cancel-timer cape-tidal--refresh-timer)))))

;;; Completion table cache

(ert-deftest cape-tidal-test-table-uses-buffer-input-not-string ()
  "Even when a non-prefix style calls with the empty string, GHCi gets the real
input.  orderless filters on its own, so it calls `all-completions' with \"\".
Querying STRING directly here would make `:complete repl 100 \"\"' and return only
the first N of all candidates (regression for the reported `d1' loss bug)."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp
                  (if (equal prefix "d1")
                      '("d1" "d10")
                    ;; With an empty prefix, the first N (uppercase-heavy) come back.
                    '("CD1" "Data.Char.ord" "Double"))))))
      (with-temp-buffer
        (insert "d1")
        (let ((table (cape-tidal--make-table (point-min) (point-max)))
              (completion-ignore-case t)
              (completion-regexp-list '("d1")))
          (let ((got (all-completions "" table)))
            (should (equal fetches '("d1")))
            (should (member "d1" got))
            (should-not (member "CD1" got))))))))

(ert-deftest cape-tidal-test-table-no-fetch-on-boundaries ()
  "`completion-boundaries' does not query GHCi."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("d1")))))
      (with-temp-buffer
        (insert "d1")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (completion-boundaries "" table nil "")
          (should (null fetches)))))))

(ert-deftest cape-tidal-test-table-empty-input-no-fetch ()
  "Do not query when the input is empty (it would only return the first N of all)."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("x")))))
      (with-temp-buffer
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (should (null (all-completions "" table)))
          (should (null fetches)))))))

(ert-deftest cape-tidal-test-table-exhaustive-filters-locally ()
  "When fully fetched (printed = total), extending the input is handled by local filtering."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("stut" "stutter" "stutWith")))))
      (with-temp-buffer
        (insert "st")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (should (equal (all-completions "st" table)
                         '("stut" "stutter" "stutWith")))
          (should (equal fetches '("st")))
          ;; Do not refetch when the action kind changes.
          (try-completion "st" table)
          (test-completion "stut" table)
          (should (equal fetches '("st")))
          ;; Extending the input is local filtering too.
          (cape-tidal-test--type "utt")
          (should (equal (all-completions "stutt" table) '("stutter")))
          (should (equal fetches '("st"))))))))

(ert-deftest cape-tidal-test-table-refetches-on-overflow ()
  "When printed < total (overflow), refetch as the input extends."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (if (equal prefix "s")
                     (cape-tidal-test--resp '("s1" "s2" "s3") 9)
                   (cape-tidal-test--resp '("stut"))))))
      (with-temp-buffer
        (insert "s")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (all-completions "s" table)
          (should (equal fetches '("s")))
          (cape-tidal-test--type "t")
          (should (equal (all-completions "st" table) '("stut")))
          (should (equal fetches '("st" "s")))
          ;; Do not refetch for the same input.
          (all-completions "st" table)
          (should (equal fetches '("st" "s")))
          ;; Refetch when the input shrinks back.
          (goto-char (point-max))
          (delete-char -1)
          (all-completions "s" table)
          (should (equal fetches '("s" "st" "s"))))))))

(ert-deftest cape-tidal-test-table-case-change-refetches ()
  "Refetch when case changes (`:complete repl' is case-sensitive)."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp
                  (if (equal prefix "D") '("Double") '("d1"))))))
      (with-temp-buffer
        (insert "d")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (all-completions "" table)
          (should (equal fetches '("d")))
          (goto-char (point-max))
          (delete-char -1)
          (insert "D")
          (should (equal (all-completions "" table) '("Double")))
          (should (equal fetches '("D" "d"))))))))

(ert-deftest cape-tidal-test-table-separator-no-refetch ()
  "Input containing a separator is not sent to GHCi; get by on existing candidates."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("d1" "d10") 99))))
      (with-temp-buffer
        (insert "d")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (all-completions "" table)
          (should (equal fetches '("d")))
          ;; ":complete repl 100 \"d 1\"" reliably yields 0, so do not send it.
          (cape-tidal-test--type " 1")
          (all-completions "" table)
          (should (equal fetches '("d"))))))))

(ert-deftest cape-tidal-test-table-failure-keeps-candidates ()
  "A fetch failure is not made exhaustive and does not erase the last successful
candidates.  Caching a failure as `a settled 0 results' kills completion for the
rest of the session."
  (let ((fetches nil) (fail nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (if fail
                     (list :ok nil)
                   (cape-tidal-test--resp '("d1" "d10") 99)))))
      (with-temp-buffer
        (insert "d")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (should (equal (all-completions "" table) '("d1" "d10")))
          (setq fail t)
          (cape-tidal-test--type "1")
          ;; Even on failure the last candidates remain.
          (should (equal (all-completions "" table) '("d1" "d10")))
          (should (equal fetches '("d1" "d")))
          ;; Do not resend a failure repeatedly for the same input.
          (all-completions "" table)
          (should (equal fetches '("d1" "d")))
          ;; Retry when the input changes (the recovery path).
          (setq fail nil)
          (cape-tidal-test--type "0")
          (all-completions "" table)
          (should (equal fetches '("d10" "d1" "d"))))))))

(ert-deftest cape-tidal-test-table-dead-buffer-no-fetch ()
  "When the origin buffer is killed, return no candidates without fetching."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("d1")))))
      (let ((buf (generate-new-buffer " *cape-tidal-marker*"))
            (table nil))
        (with-current-buffer buf
          (insert "d1")
          (setq table (cape-tidal--make-table (point-min) (point-max))))
        (kill-buffer buf)
        (should (null (all-completions "" table)))
        (should (null fetches))))))

(ert-deftest cape-tidal-test-table-queries-origin-buffer ()
  "Even with another buffer current, the query is taken between the origin buffer's markers."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("d1")))))
      (let ((buf (generate-new-buffer " *cape-tidal-origin*"))
            (table nil))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (insert "d1")
                (setq table (cape-tidal--make-table (point-min) (point-max))))
              (with-temp-buffer
                (insert "zzz")
                (all-completions "" table))
              (should (equal fetches '("d1"))))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))))))

(ert-deftest cape-tidal-test-parse-complete-escape-forms ()
  "Decodes the various escapes that GHC's `show' emits."
  (let ((r (cape-tidal--parse-complete-output
            (concat "6 6 \"\"\n"
                    "\"\\955\"\n"      ; decimal (Unicode identifier)
                    "\"\\x42\"\n"      ; hex
                    "\"\\o101\"\n"     ; octal
                    "\"\\SOH\"\n"      ; mnemonic (longest match)
                    "\"\\SO\\&H\"\n"   ; separation via \\&
                    "\"a\\tb\"\n"))))
    (should (plist-get r :ok))
    (should (equal (plist-get r :candidates)
                   (list (string 955) "B" "A" (string 1) (string 14 ?H) "a\tb")))))

(ert-deftest cape-tidal-test-parse-complete-rejects-bad-escapes ()
  "An uninterpretable escape or an unescaped quote fails rather than silently transforming."
  ;; An unknown escape (it used to drop the \\ and become a different string).
  (should-not (plist-get (cape-tidal--parse-complete-output "1 1 \"\"\n\"\\q\"\n") :ok))
  ;; An unescaped quote in the body.
  (should-not (plist-get (cape-tidal--parse-complete-output "1 1 \"\"\n\"a\"junk\"\n") :ok))
  ;; An escape cut off at the end.
  (should-not (plist-get (cape-tidal--parse-complete-output "1 1 \"\"\n\"a\\\"\n") :ok)))

(ert-deftest cape-tidal-test-parse-complete-header-invariants ()
  "Verify the header invariants."
  ;; printed > total is impossible.
  (should-not (plist-get (cape-tidal--parse-complete-output "2 1 \"\"\n\"a\"\n\"b\"\n") :ok))
  ;; printed = 0 with total > 0 is valid but not exhaustive.
  (let ((r (cape-tidal--parse-complete-output "0 7 \"\"\n")))
    (should (plist-get r :ok))
    (should (null (plist-get r :candidates)))
    (should (/= (plist-get r :printed) (plist-get r :total)))))

(ert-deftest cape-tidal-test-encode-ghci-string ()
  "The prefix passed to GHCi is escaped as a Haskell string."
  (should (equal (cape-tidal--encode-ghci-string "d1") "\"d1\""))
  ;; When trying to complete Haskell's \\ operator.
  (should (equal (cape-tidal--encode-ghci-string (string ?\\))
                 (concat "\"" (make-string 2 ?\\) "\"")))
  (should (equal (cape-tidal--encode-ghci-string "a\"b") "\"a\\\"b\""))
  (should (equal (cape-tidal--encode-ghci-string "a\nb") "\"a\\nb\"")))

(ert-deftest cape-tidal-test-get-candidates-encodes-prefix ()
  "The prefix is escaped in the request string that is sent."
  (let ((sent nil))
    (cl-letf (((symbol-function 'cape-tidal--send-request)
               (lambda (request _cb &optional _q) (push request sent))))
      (cape-tidal--get-candidates (string ?\\) #'ignore)
      (should (equal sent
                     (list (format ":complete repl %d \"%s\""
                                   cape-tidal-candidates-limit
                                   (make-string 2 ?\\))))))))

(ert-deftest cape-tidal-test-table-invalid-marker-drops-cache ()
  "Even with a success cache, return no candidates once the markers become invalid."
  (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
             (lambda (_prefix) (cape-tidal-test--resp '("d1" "d10")))))
    (let ((buf (generate-new-buffer " *cape-tidal-stale*"))
          (table nil))
      (with-current-buffer buf
        (insert "d1")
        (setq table (cape-tidal--make-table (point-min) (point-max))))
      (should (equal (all-completions "" table) '("d1" "d10")))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      ;; It is no longer known which input the candidates correspond to.
      (should (null (all-completions "" table)))
      (should (null (try-completion "" table)))
      (should (null (test-completion "d1" table))))))

(ert-deftest cape-tidal-test-table-emptied-region-returns-nothing ()
  "Once the completion region is empty, do not keep returning the last candidates."
  (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
             (lambda (_prefix) (cape-tidal-test--resp '("d1" "d10")))))
    (with-temp-buffer
      (insert "d1")
      (let ((table (cape-tidal--make-table (point-min) (point-max))))
        (should (equal (all-completions "" table) '("d1" "d10")))
        (delete-region (point-min) (point-max))
        ;; Do not query on empty input.  The last candidates remain for local filtering.
        (should (equal (all-completions "d1" table) '("d1" "d10")))))))

(ert-deftest cape-tidal-test-table-separator-without-cache ()
  "Do not send to GHCi when separator input arrives with no cache."
  (let ((fetches nil))
    (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
               (lambda (prefix)
                 (push prefix fetches)
                 (cape-tidal-test--resp '("x")))))
      (with-temp-buffer
        (insert "d 1")
        (let ((table (cape-tidal--make-table (point-min) (point-max))))
          (should (null (all-completions "" table)))
          (should (null fetches)))))))

(ert-deftest cape-tidal-test-table-survives-narrowing ()
  "Do not error even when the completion region is outside the current narrowing."
  (cl-letf (((symbol-function 'cape-tidal--fetch-candidates-sync)
             (lambda (prefix)
               (cape-tidal-test--resp (list (concat prefix "!"))))))
    (with-temp-buffer
      (insert "abc d1 xyz")
      (let ((table (cape-tidal--make-table 5 7)))
        (narrow-to-region 1 4)
        (should (equal (all-completions "" table) '("d1!")))))))

;;; Sentinel and restart detection (uses a real process)

(ert-deftest cape-tidal-test-sentinel-resets-on-process-death ()
  "On process death the sentinel resets all state and the type cache."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc (start-process "cape-tidal-test-proc" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (should (eq cape-tidal--process proc))
      (should (eq (process-sentinel proc) #'cape-tidal--sentinel))
      ;; Set up an in-progress state.
      (setq cape-tidal--state 'active)
      (setq cape-tidal--deadline (+ (float-time) 5.0))
      (delete-process proc)
      ;; Wait for the sentinel to fire.
      (let ((deadline (+ (float-time) 2.0)))
        (while (and cape-tidal--state (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should (null cape-tidal--state))
      (should (null cape-tidal--type-cache)))))

(ert-deftest cape-tidal-test-sentinel-ignores-old-process ()
  "Even if the pre-swap process dies late, it does not take the new process's state down."
  (cape-tidal-test--with-tidal-buffer
    (let ((old (start-process "cape-tidal-test-old" (current-buffer) "cat")))
      (set-process-query-on-exit-flag old nil)
      (cape-tidal--ensure-filter-installed)
      (should (eq cape-tidal--process old))
      (let ((new (start-process "cape-tidal-test-new" (current-buffer) "cat")))
        (set-process-query-on-exit-flag new nil)
        ;; Have it detect a GHCi restart (a process swap in the same buffer).
        (cape-tidal--ensure-filter-installed)
        (should (eq cape-tidal--process new))
        ;; Create state and a type cache for the new process.
        (setq cape-tidal--state 'active)
        (setq cape-tidal--deadline (+ (float-time) 5.0))
        (puthash "d1" ":: ControlPattern -> IO ()" cape-tidal--type-cache)
        ;; The old process dies here.
        (delete-process old)
        (let ((deadline (+ (float-time) 1.0)))
          (while (< (float-time) deadline)
            (accept-process-output nil 0.05)))
        (should (eq cape-tidal--state 'active))
        (should cape-tidal--type-cache)
        (should (gethash "d1" cape-tidal--type-cache))
        (delete-process new)))))

(ert-deftest cape-tidal-test-teardown-removes-hooks-and-sentinel ()
  "Teardown restores the hooks, timers, and sentinel.
Leaving them when the function definitions vanish makes the next process output a
void-function."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc (start-process "cape-tidal-test-td" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (should (memq #'cape-tidal--preoutput-filter
                    comint-preoutput-filter-functions))
      (should (memq #'cape-tidal--comint-input-noticed
                    comint-input-filter-functions))
      (should (memq #'cape-tidal--buffer-killed kill-buffer-hook))
      (should (eq (process-sentinel proc) #'cape-tidal--sentinel))
      (setq cape-tidal--timeout-timer (run-with-timer 30 nil #'ignore))
      (cape-tidal--teardown-buffer)
      (should-not (memq #'cape-tidal--preoutput-filter
                        comint-preoutput-filter-functions))
      (should-not (memq #'cape-tidal--comint-input-noticed
                        comint-input-filter-functions))
      (should-not (memq #'cape-tidal--buffer-killed kill-buffer-hook))
      (should-not (eq (process-sentinel proc) #'cape-tidal--sentinel))
      (should (null cape-tidal--timeout-timer))
      (should (null cape-tidal--filter-installed))
      (delete-process proc))))

(ert-deftest cape-tidal-test-swap-restores-old-sentinel ()
  "On a process swap, restore the old process's composite sentinel.
Unloading with it left in place becomes a void-function when the old process dies."
  (cape-tidal-test--with-tidal-buffer
    (let ((old (start-process "cape-tidal-test-old2" (current-buffer) "cat")))
      (set-process-query-on-exit-flag old nil)
      (cape-tidal--ensure-filter-installed)
      (should (eq (process-sentinel old) #'cape-tidal--sentinel))
      (let ((new (start-process "cape-tidal-test-new2" (current-buffer) "cat")))
        (set-process-query-on-exit-flag new nil)
        (cape-tidal--ensure-filter-installed)
        (should (eq (process-sentinel new) #'cape-tidal--sentinel))
        ;; The old process is left alive.
        (should (process-live-p old))
        (should-not (eq (process-sentinel old) #'cape-tidal--sentinel))
        (delete-process old)
        (delete-process new)))))

(ert-deftest cape-tidal-test-teardown-restores-original-sentinel ()
  "If an original sentinel was set, teardown restores exactly that one."
  (cape-tidal-test--with-tidal-buffer
    (let ((original (lambda (_proc _event) 'original))
          (proc (start-process "cape-tidal-test-orig" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc original)
      (cape-tidal--ensure-filter-installed)
      (should (eq (process-sentinel proc) #'cape-tidal--sentinel))
      (cape-tidal--teardown-buffer)
      (should (eq (process-sentinel proc) original))
      (delete-process proc))))

(ert-deftest cape-tidal-test-teardown-keeps-foreign-sentinel ()
  "If another sentinel was installed later, teardown does not overwrite it."
  (cape-tidal-test--with-tidal-buffer
    (let ((foreign (lambda (_proc _event) 'foreign))
          (proc (start-process "cape-tidal-test-foreign" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (set-process-sentinel proc foreign)
      (cape-tidal--teardown-buffer)
      (should (eq (process-sentinel proc) foreign))
      (delete-process proc))))

(ert-deftest cape-tidal-test-teardown-is-idempotent ()
  "Calling teardown twice is safe."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc (start-process "cape-tidal-test-idem" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (cape-tidal--teardown-buffer)
      (cape-tidal--teardown-buffer)
      (should (null cape-tidal--filter-installed))
      (should (null cape-tidal--process))
      (delete-process proc))))

(ert-deftest cape-tidal-test-sync-backup-cleans-active-request ()
  "When giving up at the backup deadline, fold an active request down to draining."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc (start-process "cape-tidal-test-quiet" (current-buffer) "cat"))
          (prefetched 'none))
      (set-process-query-on-exit-flag proc nil)
      (cape-tidal--ensure-filter-installed)
      (cl-letf (((symbol-function 'cape-tidal--fetch-types-async)
                 (lambda (c) (setq prefetched c)))
                ;; Make an active request that never responds.  The state
                ;; machine's timer is not run, so only the backup deadline acts.
                ((symbol-function 'cape-tidal--get-candidates)
                 (lambda (_prefix callback)
                   (with-current-buffer (cape-tidal--tidal-buffer)
                     (setq cape-tidal--state 'active)
                     (setq cape-tidal--callback callback)
                     (setq cape-tidal--deadline (+ (float-time) 100))))))
        (let* ((cape-tidal-timeout 0.05)
               (res (cape-tidal--fetch-candidates-sync "d")))
          (should-not (plist-get res :ok))
          ;; Go to draining so a response on the wire is not misdelivered to the next request.
          (should (eq cape-tidal--state 'draining))
          (should (null cape-tidal--callback))
          (should (null cape-tidal--timeout-timer))
          ;; Since we gave up, no prefetch starts.
          (should (eq prefetched 'none))))
      (delete-process proc))))

(ert-deftest cape-tidal-test-sync-abandons-late-response ()
  "A response arriving after giving up at the backup deadline moves neither result nor prefetch."
  (let ((captured nil)
        (prefetched 'none))
    (cl-letf (((symbol-function 'cape-tidal--get-candidates)
               (lambda (_prefix callback) (setq captured callback)))
              ((symbol-function 'cape-tidal--fetch-types-async)
               (lambda (c) (setq prefetched c)))
              ;; With no process, the wait is cut off immediately.
              ((symbol-function 'cape-tidal--tidal-buffer) (lambda () nil)))
      (let ((res (cape-tidal--fetch-candidates-sync "d")))
        (should-not (plist-get res :ok))
        (should captured)
        ;; A success response arrives late.
        (funcall captured (list :ok t :printed 1 :total 1
                                :common-prefix "" :candidates '("d1")))
        ;; Since we gave up, no prefetch starts.
        (should (eq prefetched 'none))))))

(ert-deftest cape-tidal-test-restart-detection-resets-cache ()
  "Detect a process swap (GHCi restart) and rebuild the type cache."
  (cape-tidal-test--with-tidal-buffer
    (let ((proc1 (start-process "cape-tidal-test-p1" (current-buffer) "cat")))
      (set-process-query-on-exit-flag proc1 nil)
      (cape-tidal--ensure-filter-installed)
      (puthash "stut" ":: A" cape-tidal--type-cache)
      (delete-process proc1)
      (let ((proc2 (start-process "cape-tidal-test-p2" (current-buffer) "cat")))
        (set-process-query-on-exit-flag proc2 nil)
        (cape-tidal--ensure-filter-installed)
        (should (eq cape-tidal--process proc2))
        (should (eq (process-sentinel proc2) #'cape-tidal--sentinel))
        ;; The old process's type cache does not remain.
        (should (hash-table-p cape-tidal--type-cache))
        (should-not (gethash "stut" cape-tidal--type-cache))
        (delete-process proc2)))))

;;; Integration tests (fake GHCi + real comint)
;;
;; Define a replica with the same behavior as tidal.el's tidal-send-string
;; (appends "\n", errors on a dead process), run fake-ghci.sh through comint, and
;; verify end-to-end through real process output, chunk splitting, and timers.

(defun cape-tidal-test--tidal-send-string-impl (s)
  "A replica of tidal.el's `tidal-send-string'."
  (if (comint-check-proc tidal-buffer)
      (comint-send-string tidal-buffer (concat s "\n"))
    (error "No tidal process running?")))

(defun cape-tidal-test--wait-for (pred timeout)
  "Wait up to TIMEOUT seconds until PRED returns non-nil.
The result is the last value of PRED."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro cape-tidal-test--with-fake-ghci (&rest body)
  "Start a fake GHCi through comint and run BODY.
Guarantees the `tidal-send-string' replica definition and the advice teardown."
  (declare (indent 0) (debug t))
  `(let ((tidal-buffer "*cape-tidal-fake*")
         (script (expand-file-name "fake-ghci.sh" cape-tidal-test--dir)))
     (unwind-protect
         (progn
           (defalias 'tidal-send-string #'cape-tidal-test--tidal-send-string-impl)
           (make-comint-in-buffer "cape-tidal-fake" tidal-buffer "sh" nil script)
           (with-current-buffer tidal-buffer
             (set-process-query-on-exit-flag
              (get-buffer-process (current-buffer)) nil))
           ;; Wait for the initial prompt.
           (should (cape-tidal-test--wait-for
                    (lambda ()
                      (with-current-buffer tidal-buffer
                        (string-match-p "tidal>" (buffer-string))))
                    3.0))
           (cape-tidal--ensure-filter-installed)
           ,@body)
       (cape-tidal--remove-advice)
       (fmakunbound 'tidal-send-string)
       (let ((buf (get-buffer "*cape-tidal-fake*")))
         (when buf
           (with-current-buffer buf (cape-tidal--cancel-timeout))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf)))))))

(ert-deftest cape-tidal-test-integration-no-candidate-flood ()
  "Running candidate fetch and type prefetch end-to-end does not leak into comint."
  (skip-unless (executable-find "sh"))
  (cape-tidal-test--with-fake-ghci
    ;; The same synchronous fetch as the real capf path (the type prefetch
    ;; starts once it finishes).
    (let ((r (cape-tidal--fetch-candidates-sync "st")))
      (should (plist-get r :ok))
      (should (equal (plist-get r :candidates) '("stut" "stutter" "stutWith"))))
    ;; Wait for all three prefetches to finish.  An annotation query reorders
    ;; them as a demand hint, so do not judge by looking at just one.
    (should (cape-tidal-test--wait-for
             (lambda ()
               (and (cape-tidal--annotation-function "stut")
                    (cape-tidal--annotation-function "stutter")
                    (cape-tidal--annotation-function "stutWith")))
             3.0))
    (should (equal (cape-tidal--annotation-function "stut")
                   ":: Pattern String "))
    (accept-process-output nil 0.3)
    ;; Neither candidates nor types leaked into the comint buffer (regression).
    (with-current-buffer tidal-buffer
      (should-not (string-match-p "stut" (buffer-string)))
      (should-not (string-match-p "Pattern" (buffer-string))))))

(ert-deftest cape-tidal-test-integration-eval-output-never-hidden ()
  "Output of an evaluation that interrupts during completion always shows in comint."
  (skip-unless (executable-find "sh"))
  (cape-tidal-test--with-fake-ghci
    (let ((got 'unset))
      ;; A user evaluation interrupts right after the completion request.
      (cape-tidal--get-candidates "st" (lambda (r) (setq got r)))
      (tidal-send-string "d1 $ sound \"bd\"")
      ;; Completion loses immediately and gets a failure response (not a successful 0).
      (should-not (eq got 'unset))
      (should-not (plist-get got :ok))
      ;; The evaluation output is not hidden.
      (should (cape-tidal-test--wait-for
               (lambda ()
                 (with-current-buffer tidal-buffer
                   (string-match-p "played" (buffer-string))))
               3.0)))))

(ert-deftest cape-tidal-test-integration-timeout-fail-open ()
  "Even with GHCi unresponsive, the REPL does not go silent (fail open)."
  (skip-unless (executable-find "sh"))
  (cape-tidal-test--with-fake-ghci
    (let ((cape-tidal-timeout 0.5)
          (cape-tidal-drain-timeout 0.3)
          (got 'unset))
      ;; The fake responds to "slow" after 3 seconds.
      (cape-tidal--send-request "slow" (lambda (r) (setq got r)) t)
      ;; On timeout the callback gets nil.
      (should (cape-tidal-test--wait-for (lambda () (not (eq got 'unset))) 2.0))
      (should (null got))
      ;; The late response returns to the raw comint after the drain deadline.
      (should (cape-tidal-test--wait-for
               (lambda ()
                 (with-current-buffer tidal-buffer
                   (string-match-p "slow done" (buffer-string))))
               5.0)))))

(provide 'cape-tidal-test)
;;; cape-tidal-test.el ends here
