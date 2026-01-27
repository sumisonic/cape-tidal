;;; e2e-real-tidal.el --- End-to-end tests with real GHCi and Tidal -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Drive the capf through a real GHCi, real Tidal, and a real completion style,
;; not a fake GHCi.  The reported bug where `d1' does not appear as a candidate
;; under orderless happens because a non-prefix completion style passes the empty
;; string to the completion table, so it does not reproduce in a unit test that
;; calls plain `all-completions' directly.  Here we go through
;; `completion-all-completions' so the real completion style is exercised.
;;
;; Example run:
;;
;;   CAPE_TIDAL_E2E_LOAD_PATH=/path/to/haskell-mode:/path/to/tidal:/path/to/orderless \
;;     emacs -Q --batch -L . -l cape-tidal.el -l test/e2e-real-tidal.el \
;;           -f ert-run-tests-batch-and-exit
;;
;; Where ghci, tidal.el, or orderless is missing, all tests are skipped.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'cape-tidal)

;; The variables of tidal.el / haskell-mode become special only inside the file
;; that wrote a valueless `defvar'.  They are not loaded yet at compile time, so
;; declare them here too.
(defvar tidal-buffer)
(declare-function tidal-mode "tidal")
(declare-function tidal-send-string "tidal" (s))

(defvar tidal-interpreter)
(defvar tidal-interpreter-arguments)
(defvar tidal-boot-script-path)
(defvar tidal-mode-hook)
(defvar haskell-mode-hook)

(dolist (dir (let ((env (getenv "CAPE_TIDAL_E2E_LOAD_PATH")))
               (and env (split-string env ":" t))))
  (add-to-list 'load-path dir))

;; Some versions of haskell-mode reference an old flymake variable, so load it
;; first.  tidal-mode calls interactive-haskell-mode, which the e2e does not
;; need, so a stub is enough.
(require 'flymake nil t)
(unless (fboundp 'interactive-haskell-mode)
  (defun interactive-haskell-mode (&optional _arg)
    "Test stub."
    nil))

(defconst cape-tidal-e2e--available
  (and (executable-find "ghci")
       (require 'haskell-mode nil t)
       (require 'tidal nil t)
       (require 'orderless nil t)
       (stringp (bound-and-true-p tidal-boot-script-path))
       (file-readable-p tidal-boot-script-path)
       t)
  "Non-nil in an environment where the real-Tidal e2e can run.")

(defvar cape-tidal-e2e--booted nil
  "Non-nil once the session is started.  One GHCi is reused across tests.")

(defun cape-tidal-e2e--wait-for (pred timeout)
  "Wait up to TIMEOUT seconds until PRED returns non-nil."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defun cape-tidal-e2e--boot ()
  "Start a real GHCi and load Tidal.  Do nothing if already started."
  (unless (and cape-tidal-e2e--booted
               (cape-tidal--live-process (get-buffer tidal-buffer)))
    (when (get-buffer tidal-buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer tidal-buffer)))
    ;; Same steps as tidal.el's `tidal-start-haskell' (only the window
    ;; manipulation is left out).
    (apply #'make-comint-in-buffer "tidal" tidal-buffer tidal-interpreter nil
           tidal-interpreter-arguments)
    (tidal-send-string (concat ":script " tidal-boot-script-path))
    (with-current-buffer tidal-buffer
      (let ((deadline (+ (float-time) 180)))
        (while (and (< (float-time) deadline)
                    (not (save-excursion
                           (goto-char (point-max))
                           (re-search-backward "^tidal> " nil t))))
          (accept-process-output (get-buffer-process (current-buffer)) 0.2))))
    (setq cape-tidal-e2e--booted t))
  (should (cape-tidal--live-process (get-buffer tidal-buffer))))

(defmacro cape-tidal-e2e--with-input (text &rest body)
  "Run BODY with TEXT inserted in a tidal-mode buffer.
From BODY, `candidates' gives the candidates through the real completion style."
  (declare (indent 1) (debug t))
  `(let ((buf (generate-new-buffer " *cape-tidal-e2e-src*")))
     (unwind-protect
         (with-current-buffer buf
           (let ((tidal-mode-hook nil) (haskell-mode-hook nil) (prog-mode-hook nil))
             (tidal-mode))
           (insert ,text)
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (kill-buffer buf)))))

(defun cape-tidal-e2e--candidates ()
  "Return the candidate list, through the completion style, at point.
Uses the current buffer and point."
  (let ((completion-styles '(orderless basic))
        (completion-category-defaults nil)
        (completion-category-overrides nil))
    (let ((res (cape-tidal)))
      (should res)
      (let* ((beg (nth 0 res))
             (table (nth 2 res))
             (input (buffer-substring-no-properties beg (point)))
             (all (completion-all-completions input table nil (length input))))
        ;; The return value of `completion-all-completions' may have a non-nil cdr at the tail.
        (when (consp all)
          (let ((tail (last all)))
            (unless (listp (cdr tail))
              (setcdr tail nil))))
        (mapcar #'substring-no-properties all)))))

;;; Tests

(ert-deftest cape-tidal-e2e-completes-d1-from-single-letter ()
  "`d1' appears among the candidates as soon as `d' is typed (regression for the reported bug).
GHCi has more than 100 candidates for `d', so the overflow handling is verified
at the same time."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (cape-tidal-e2e--with-input "d"
    (let ((cands (cape-tidal-e2e--candidates)))
      (should (member "d1" cands))
      ;; If the empty-prefix response was grabbed, uppercase qualified names line up.
      (should-not (member "CD1" cands))
      (should-not (member "Data.Char.ord" cands)))))

(ert-deftest cape-tidal-e2e-completes-d1-when-fully-typed ()
  "`d1' stays among the candidates even when `d1' is fully typed.

Note: it is the `d' test above that discriminates the regression.  When
`completion-styles' is (orderless basic), basic is not tried if orderless
returns even one result, but falls back to basic on zero results.  The longer the
input, the more likely orderless returns zero, so a broken implementation can
pass by chance via basic."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (cape-tidal-e2e--with-input "d1"
    (let ((cands (cape-tidal-e2e--candidates)))
      (should (member "d1" cands))
      (should-not (member "CD1" cands)))))

(ert-deftest cape-tidal-e2e-no-candidate-flood ()
  "Candidates do not leak into the comint buffer even when completion is repeated."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (with-current-buffer tidal-buffer (erase-buffer))
  (dolist (text '("d" "d1" "so" "stut"))
    (cape-tidal-e2e--with-input text
      (cape-tidal-e2e--candidates)))
  (accept-process-output nil 0.3)
  (with-current-buffer tidal-buffer
    ;; A raw GHCi candidate line is "a whole line wrapped in quotes".  Limiting to
    ;; identifier form would miss a leak of operator candidates or candidates
    ;; containing escapes, so look at the quoted line itself.
    (should-not (string-match-p "^\"[^\n]*\"$" (buffer-string)))))

(ert-deftest cape-tidal-e2e-eval-output-never-hidden ()
  "Sending a pattern evaluation right after completion does not swallow its output."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (with-current-buffer tidal-buffer (erase-buffer))
  (cape-tidal-e2e--with-input "d"
    (cape-tidal--get-candidates "d" #'ignore)
    ;; Make it print a distinctive string.  A common value like "42" could pass
    ;; on unrelated output (a false positive).
    (tidal-send-string "putStrLn \"cape-tidal-e2e-eval-marker\""))
  (with-current-buffer tidal-buffer
    (should (cape-tidal-e2e--wait-for
             (lambda ()
               (string-match-p "^cape-tidal-e2e-eval-marker$" (buffer-string)))
             10.0))))

(ert-deftest cape-tidal-e2e-annotation-follows-visible-candidates ()
  "Types are attached first to the candidates on screen (those whose annotation was requested).
GHCi returns candidates in alphabetical order, but corfu sorts by length for
display.  The fetch order used to be fixed to the first 20 in GHCi order, so a
candidate high on screen could go without a type (reported: no type on
`setupOctatrack' etc.)."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (cape-tidal-e2e--with-input "d"
    (let* ((cands (cape-tidal-e2e--candidates))
           ;; Pretend that candidates at position 29 and later in alphabetical
           ;; order (outside the default budget of 20) are "on screen".
           (visible (seq-filter (lambda (c)
                                  (member c '("degrade" "degradeBy" "discretise" "distort")))
                                cands)))
      (should (>= (length visible) 3))
      ;; As corfu does, request the annotation of the visible candidates (demand hint).
      (dolist (c visible)
        (cape-tidal--annotation-function c))
      ;; The visible candidates get types even though they were outside the budget.
      (should (cape-tidal-e2e--wait-for
               (lambda ()
                 (cl-every #'cape-tidal--annotation-function visible))
               10.0))
      (should (string-prefix-p ":: " (cape-tidal--annotation-function "degrade"))))))

(ert-deftest cape-tidal-e2e-completion-right-after-block-eval ()
  "Completing right after a block evaluation (`:{' … `:}') returns candidates without waiting.
tidal.el sends a multi-line evaluation as three `tidal-send-string' calls, but
GHCi returns one prompt.  Counting all three stalls the foreign-send counter, so
the completion pending did not go through until the forced recovery (2 seconds)."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  ;; Install the advice (normally installed on the first completion).
  (cape-tidal--ensure-filter-installed)
  ;; The same send shape as tidal.el's `tidal-eval-multiple-lines'.
  (tidal-send-string ":{")
  (tidal-send-string "let cape_tidal_e2e_block = 1 + 1")
  (tidal-send-string ":}")
  (accept-process-output nil 0.3)
  (cape-tidal-e2e--with-input "d"
    (let* ((t0 (float-time))
           (cands (cape-tidal-e2e--candidates))
           (elapsed (- (float-time) t0)))
      (should (member "d1" cands))
      ;; It returned without waiting for the forced recovery.
      (should (< elapsed 1.0)))))

(ert-deftest cape-tidal-e2e-type-follows-narrowing-toward-one ()
  "A candidate high on screen gets a type even while narrowing down to a single one.
When candidates shrink through local filtering with no GHCi refetch, the demand
hints for candidates visible under the previous input linger and wait their turn
for the budget, so the target type appears late.  This is the regression test.
Extend `set' to `setc' and check that `setcps' gets a type."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (let ((buf (generate-new-buffer " *cape-tidal-e2e-narrow*"))
        (table nil))
    (unwind-protect
        (with-current-buffer buf
          (let ((tidal-mode-hook nil) (haskell-mode-hook nil) (prog-mode-hook nil))
            (tidal-mode))
          (insert "s")
          (let ((completion-styles '(orderless basic)))
            (setq table (nth 2 (cape-tidal)))
            ;; s -> se -> set -> setc, extending within the same completion session.
            (dolist (more '("e" "t" "c"))
              (goto-char (point-max))
              (insert more)
              (let* ((input (buffer-substring-no-properties (point-min) (point-max)))
                     (all (completion-all-completions input table nil (length input))))
                (when (and (consp all) (not (listp (cdr (last all)))))
                  (setcdr (last all) nil))
                ;; As corfu does, request the annotation of the visible candidates.
                (dolist (c all) (cape-tidal--annotation-function c)))))
          ;; The budget goes to the candidates high on screen, so it attaches
          ;; within a few hundred ms without waiting.
          (should (cape-tidal-e2e--wait-for
                   (lambda () (cape-tidal--annotation-function "setcps"))
                   3.0))
          (should (string-prefix-p ":: " (cape-tidal--annotation-function "setcps"))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest cape-tidal-e2e-survives-process-death ()
  "Completion does not error even when only the GHCi process dies."
  (skip-unless cape-tidal-e2e--available)
  (cape-tidal-e2e--boot)
  (let ((proc (get-buffer-process (get-buffer tidal-buffer))))
    (delete-process proc))
  (accept-process-output nil 0.2)
  (cape-tidal-e2e--with-input "d"
    ;; With no process the capf itself returns nil (does not error).
    (should-not (cape-tidal)))
  ;; Restart for the next test.
  (setq cape-tidal-e2e--booted nil))

(provide 'e2e-real-tidal)
;;; e2e-real-tidal.el ends here
