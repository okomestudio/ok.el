;;; ok.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;
;; Version: 0.1
;;
;;; Commentary:
;;
;; Okome Studio utilities.
;;
;;; Code:

(require 'ok-buffer)
(require 'ok-edit)
(require 'ok-face)
(require 'ok-file)
(require 'ok-prompt)
(require 'ok-sys)
(require 'ok-web)

(defun ok-debug-message (text)
  "Print a debug message TEXT with timestamp."
  (message "%s %s"
           (format-time-string "%FT%H:%M:%S.%3N" (current-time))
           text))

(defun ok-prepend-timestamp (format-string &rest args)
  "Prepend timestamp to `message' output.
FORMAT-STRING and ARGS are passed through. See
https://emacs.stackexchange.com/a/38511/599."
  (unless (string-equal format-string "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (insert (format-time-string "%FT%H:%M:%S.%3N" (current-time)) " ")))))

(advice-add 'message :before 'ok-prepend-timestamp)

(defun ok-execution-time (name &rest body)
  "Measure execution time of BODY named NAME."
  (let ((t0 (float-time)))
    (unwind-protect
        (progn body)
      (message "%s ran in %f" name (- (float-time) t0)))))

(defmacro ok-safe-local-variable-add (&rest pairs)
  "Add variable-function PAIRS to `safe-local-variable'."
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable-value pairs"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Not a symbol: %s" (car pairs)))
      (setq expr
            (cons (list 'put
                        (list 'quote (car pairs))
                        (list 'quote 'safe-local-variable)
                        (list 'function (car (cdr pairs))))
                  expr))
      (setq pairs (cdr (cdr pairs))))
    (macroexp-progn (nreverse expr))))

(provide 'ok)
;;; ok.el ends here
