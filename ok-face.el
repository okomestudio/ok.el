;;; ok-face.el --- Okome Studio face module  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ok-font)

(defun ok-face-color-scale (color factor)
  "Scale RGB COLOR lighter or darker by a numeric FACTOR.
FACTOR is between 0.0 to 2.0, where 1.0 means unchanged.

Example:

  (ok-face--color-scale \"#554433\" 0.0)  ;; black
  (ok-face--color-scale \"#554433\" 0.8)  ;; darker
  (ok-face--color-scale \"#554433\" 1.0)  ;; unchanged
  (ok-face--color-scale \"#554433\" 1.2)  ;; lighter
  (ok-face--color-scale \"#554433\" 2.0)  ;; white"
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

;;; FONTSET

(defvar ok-face-lang-charsets
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

(defun ok-face-set-fontset-font (fontset lang font-family &optional frame)
  "Set a LANG subset of FONTSET to FONT-FAMILY.
This is used to create a fontset with its subset filled with
another fontset from a different language."
  (let ((charsets (cdr (assoc lang ok-face-lang-charsets))))
    (dolist (charset charsets)
      (set-fontset-font fontset charset (font-spec :family font-family) frame))))

(defun ok-face-fontset-create (fontset font-family &rest kwargs)
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
             (ok-face-set-fontset-font fontset lang font-family frame))))))

(provide 'ok-face)
;;; ok-face.el ends here
