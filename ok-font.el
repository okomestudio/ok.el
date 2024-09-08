;;; ok-font.el --- Font  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ok-file)

(defun ok-font-family-exists-p (font-family)
  "Return non-nil if font with FONT-FAMILY exists."
  (if (find-font (font-spec :family font-family))
      t
    (message "WARNING: Font for `%s' not found" font-family)))

(defun ok-font-install-from-url (url &optional dir)
  "Download a font file at URL and install it under DIR.

When not given, DIR defaults to the value of XDG_DATA_HOME
environment variable.

When a new font file is created, the function returns the path to
it."
  (let ((dir (or dir
                 (expand-file-name "fonts/"
                                   (or (getenv "XDG_DATA_HOME")
                                       "~/.local/share/")))))
    (ok-file-ensure-from-url url dir)))

(defun ok-font-installed-p (font-name)
  "Return non-nil if font with FONT-NAME is installed."
  ;; In X, `x-list-fonts' was used
  (find-font (font-spec :name font-name)))

(defun ok-font-cache-update ()
  "Update font cache."
  (async-shell-command "fc-cache -f -v"))

(provide 'ok-font)
;;; ok-font.el ends here
