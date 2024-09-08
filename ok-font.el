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

;;; FONTSET

(defvar ok-fontset-lang-charsets
  '((ja japanese-jisx0208
        japanese-jisx0208-1978
        japanese-jisx0212
        japanese-jisx0213-1
        japanese-jisx0213-2
        japanese-jisx0213-a
        japanese-jisx0213.2004-1
        jisx0201
        latin-jisx0201
        katakana-jisx0201
        katakana-sjis))
  "Mapping of language code to the language charsets.")

(defun ok-fontset-set-font (fontset lang font-family &optional frame)
  "Set a LANG subset of FONTSET to FONT-FAMILY.
This is used to create a fontset with its subset filled with
another fontset from a different language."
  (let ((charsets (cdr (assoc lang ok-fontset-lang-charsets))))
    (dolist (charset charsets)
      (set-fontset-font fontset charset (font-spec :family font-family) frame))))

(defun ok-fontset-create (fontset font-family &rest kwargs)
  "Create FONTSET using FONT-FAMILY.

KWARGS take the following arguments:

:frame - The frame with which the fontset is associated.
:subsets - A list of (language font-family) pair.

When the subsets are given, they are used to set the language
subsets to the corresponding font-family."
  (let ((frame (plist-get kwargs :frame))
        (subsets (plist-get kwargs :subsets)))
    (ok-font-family-exists-p font-family)
    (create-fontset-from-fontset-spec
     (font-xlfd-name (font-spec :family font-family :registry fontset)))
    (dolist (subset subsets)
      (let ((lang (car subset))
            (font-family (car (cdr subset))))
        (and (ok-font-family-exists-p font-family)
             (ok-fontset-set-font fontset lang font-family frame))))))

(provide 'ok-font)
;;; ok-font.el ends here
