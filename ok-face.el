;;; ok-face.el --- Okome Studio face module  -*- lexical-binding: t -*-
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
;;; Code:

(defun ok-face-color-scale (color factor)
  "Scale RGB COLOR lighter or darker by a numeric FACTOR.
FACTOR is between 0.0 to 2.0, where 1.0 means unchanged.

Example:

  (ok-face-color-scale \"#554433\" 0.0)  ;; black
  (ok-face-color-scale \"#554433\" 0.8)  ;; darker
  (ok-face-color-scale \"#554433\" 1.0)  ;; unchanged
  (ok-face-color-scale \"#554433\" 1.2)  ;; lighter
  (ok-face-color-scale \"#554433\" 2.0)  ;; white"
  (if (stringp color)
      (setq color (color-name-to-rgb color)))
  (let ((result 0.0))
    (cl-loop
     for pow downfrom 2
     for x in color
     do
     (setq result
           (+ result
              (* (expt 256 pow)
                 (round (* 255
                           (min 1.0
                                (max 0.0
                                     (if (< factor 1.0)
                                         (* x factor)
                                       (+ x (* (- 1.0 x) (- factor 1.0))))))))))))
    (format "#%06X" result)))

(defun ok-face-text-scale-mode-height ()
  "Get the default face height if `text-scale-mode' is active."
  (when (bound-and-true-p text-scale-mode)
    (cadr (assoc :height (cdr (assoc 'default face-remapping-alist))))))

(provide 'ok-face)
;;; ok-face.el ends here
