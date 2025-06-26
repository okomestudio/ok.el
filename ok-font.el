;;; ok-font.el --- Font  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;; Code:

(require 'ok-file)

(defun ok-font-family-exists-p (font-family)
  "Return non-nil if font with FONT-FAMILY exists."
  (if (find-font (font-spec :family font-family))
      t
    (message "WARNING: Font for `%s' not found" font-family)))

(defun ok-font-install-from-url (url &optional dir)
  "Download a font file at URL and install as a file in DIR.
When not given, DIR defaults to '$XDG_DATA_HOME/fonts/' or
'~/.local/share/fonts/' if the environment variable XDG_DATA_HOME is not
set.

The function returns a file path when a new file is created, or nil if no
new file is created."
  (ok-file-ensure-from-url url
                           (or dir
                               (expand-file-name "fonts/"
                                                 (or (getenv "XDG_DATA_HOME")
                                                     "~/.local/share/")))))

(defun ok-font-installed-p (font-name)
  "Return non-nil if font with FONT-NAME is installed."
  ;; In X, `x-list-fonts' was used
  (find-font (font-spec :name font-name)))

(defun ok-font-cache-update ()
  "Update font cache."
  (async-shell-command "fc-cache -f -v"))

;;; Fontset

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

(defun ok-fontset-set-font (fontset lang font-spec &optional frame)
  "Set a LANG subset of FONTSET to FONT-SPEC.
This is used to create a fontset with its subset filled with
another fontset from a different language."
  (let ((charsets (cdr (assoc lang ok-fontset-lang-charsets))))
    (dolist (charset charsets)
      (set-fontset-font fontset charset font-spec frame))))

(cl-defun ok-fontset-create (fontset
                             font-family
                             &key
                             (frame nil)
                             (subsets nil)
                             (char-specs nil))
  "Create FONTSET using FONT-FAMILY.
The optional keyword arguments are:

  - FRAME: The frame with which the fontset is associated
  - SUBSETS: A list of (language . font-spec) pairs
  - CHAR-SPECS: A list of (character . font-spec) pairs

When the subsets are given, they are used to set the language subsets to
the corresponding font-spec."
  (ok-font-family-exists-p font-family)
  (create-fontset-from-fontset-spec
   (font-xlfd-name (font-spec :family font-family :registry fontset)))
  (pcase-dolist (`(,lang . ,spec) subsets)
    (ok-fontset-set-font fontset lang spec frame))
  (pcase-dolist (`(,char . ,spec) char-specs)
    (set-fontset-font fontset char spec)))

(provide 'ok-font)
;;; ok-font.el ends here
