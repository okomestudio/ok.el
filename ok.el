;;; ok.el --- Okome Studio utilities  -*- lexical-binding: t -*-
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
  (message "IN EXECUTION TIME")
  (let ((t0 (float-time)))
    (unwind-protect
        (progn body)
      (message "%s ran in %f" name (- (float-time) t0)))))

(provide 'ok)
;;; ok.el ends here
