;;; ok-face.el --- Okome Studio face module  -*- lexical-binding: t -*-
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
    (format "#%X" result)))

(defun ok-face-text-scale-mode-height ()
  "Get the default face height if `text-scale-mode' is active."
  (when (bound-and-true-p text-scale-mode)
    (car (cdr (assoc :height (cdr (assoc 'default face-remapping-alist)))))))

(provide 'ok-face)
;;; ok-face.el ends here
