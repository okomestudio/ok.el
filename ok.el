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
  (message "%s: %s"
           (format-time-string "%FT%H:%M:%S.%3N" (current-time))
           text))

(defun ok-execution-time (name &rest body)
  "Measure execution time of BODY named NAME."
  (message "IN EXECUTION TIME")
  (let ((t0 (float-time)))
    (unwind-protect
        (progn body)
      (message "%s ran in %f" name (- (float-time) t0)))))

(provide 'ok)
;;; ok.el ends here
