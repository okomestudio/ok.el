;;; ok-desktop.el --- ok-desktop  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
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
;;
;; This library extends the builtin `desktop' feature.
;;
;;; Code:

(require 'desktop)

;;; The Serde Layer

;; The extension here adds a serialization/deserialization layer for
;; global variables to be stored through desktop save.
;;
;; For a global variable stored in `desktop-globals-to-save',
;; serializer/deserializer functions can be registered in
;; `ok-desktop-global-var-serdes-funs'. The layer runs on each desktop
;; save or read.

(defvar ok-desktop-global-var-serdes-funs nil
  "Serialization and deserialization functions for global variables.
This should be a list of cons, (VARIABLE SERIALIZER DESERIALIZER).
VARIABLE is a symbol of a global variable in `desktop-globals-to-save'.
SERIALIZER is a function taking in a deserialized value of VARIABLE.
DESERIALIZER is a function taking in a serialized value of VARIABLE.")

(defun ok-desktop-outvar--ad (fun varspec)
  "Add a data serialization layer to FUN (`desktop-outvar').
VARSPEC (a symbol) is a global variable to save as desktop. If a serde
entry exists for VARSPEC in `ok-desktop-global-var-serdes-funs', use the
serializer function to transform value before saving to the desktop
file.

Use this function as an around-advice for `desktop-outvar'."
  (if-let ((serfun
            (and (symbolp varspec)
                 (nth 1 (assq varspec ok-desktop-global-var-serdes-funs)))))
      (let ((old (symbol-value varspec)))
        (set varspec (apply serfun `(,(symbol-value varspec))))
        (apply fun `(,varspec))
        (set varspec old))
    (apply fun `(,varspec))))

(advice-add #'desktop-outvar :around #'ok-desktop-outvar--ad)

(defcustom ok-desktop-before-read-hook nil
  "Hook that runs before `desktop-read'."
  :type 'hook
  :group 'ok-desktop)

(defun ok-desktop-read--ad (fun &rest r)
  "Add a data deserialization layer after `desktop-read'.
Use this as an around advice with (apply FUN R)."
  (run-hooks 'ok-desktop-before-read-hook)
  (apply fun r)
  (pcase-dolist (`(,var ,_ ,deserfun)
                 ok-desktop-global-var-serdes-funs)
    (set var (apply deserfun `(,(symbol-value var))))))

(advice-add #'desktop-read :around #'ok-desktop-read--ad)

(provide 'ok-desktop)
;;; ok-desktop.el ends here
