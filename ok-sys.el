;;; ok-sys.el --- Okome Studio system utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun ok-sys-monitor-count ()
  "Get the number of display monitors."
  (length (display-monitor-attributes-list)))

(defun ok-sys-linux-p ()
  "Return non-nil if system is Linux-based."
  (string-equal system-type "gnu/linux"))

(defun ok-sys-macos-p ()
  "Return non-nil if system is Mac OS X-based."
  (string-equal system-type "darwin"))

(provide 'ok-sys)
;;; ok-sys.el ends here
