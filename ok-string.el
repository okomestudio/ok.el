;;; ok-string.el --- String  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
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
;; The `ok-string' module provides string utilities.
;;
;;; Code:

(defun ok-string-multibyte-string-width (s &optional scale)
  "Get multibyte (column) width of string S.
SCALE is the default column width for a multibyte
character (defaults to 2)."
  (when (null scale)
    (setq scale 2))
  (if (multibyte-string-p s)
      (let ((width (length s))
            (num-mb 0))
        (dolist (it (find-multibyte-characters s nil))
          (setq num-mb (+ num-mb (cadr it))))
        (round (+ (* scale num-mb) (- width num-mb))))
    (string-width s)))

(defun ok-string-multibyte-substring (s from to &optional scale)
  "Get substring FROM to TO of multibyte string S.
SCALE is the default column width for a multibyte
character (defaults to 2)."
  (let ((scale (or scale 2))
        (sub nil)
        (i 0)
        (j 0))
    (dolist (c (mapcar #'char-to-string s))
      (when (or (and (null sub)
                     (>= j from))
                (and sub
                     (< j to)))
        (setq sub (concat sub c)))
      (setq i (1+ i))
      (setq j (if (multibyte-string-p c)
                  (+ (* 2 mb-scale) j)
                (1+ j))))
    sub))

(provide 'ok-string)
;;; ok-string.el ends here
