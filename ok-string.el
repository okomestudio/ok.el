;;; ok-string.el --- String  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The `ok-string' module provides string utilities.
;;
;;; Code:

(require 'dash)

(defun ok-string-contains-ja-p (s)
  "Return non-nil if string S contains any Japanese characters."
  ;; CJK Unified Ideographs: U+4E00–U+9FFF (common character in CJK)
  ;; CJK Extensions: U+3400–U+4DBF, U+20000–U+2A6DF, etc.
  ;; Hangul Syllables (Korean): U+AC00–U+D7AF
  ;; Hiragana (Japanese): U+3040–U+309F
  ;; Katakana (Japanese): U+30A0–U+30FF
  ;; CJK Symbols and Punctuation: U+3000–U+303F
  (let ((chars (string-to-list s))
        (found nil))
    (dolist (char chars found)
      (when (or (and (>= char #x3040) (<= char #x309F))
                (and (>= char #x30A0) (<= char #x30FF))
                (and (>= char #x4E00) (<= char #x9FFF)))
        (setq found t)))
    found))

(defun ok-string-format (s &optional alist)
  "The `format' function with named fields.
The string S is the format control string. See the documentation
for `format'. ALIST is an association list with mapping from the
field name to the field value."
  (let ((start 0) arglist field-name spec val)
    (while-let ((start (string-match
                        "%\\(([A-Za-z-]+)\\)-?[0-9]\\{0,\\}\\([cdefgosSxX]\\)"
                        s start)))
      (setq field-name (substring (match-string 1 s) 1 -1))
      (setq spec (match-string 2 s))
      (setq val (alist-get (intern field-name) alist))
      (setq arglist (append arglist `(,val)))
      (setq s (replace-match (concat "%" spec) nil nil s)))
    (apply #'format `(,s ,@arglist))))

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
                  (+ (* scale) j)
                (1+ j))))
    sub))

(defun ok-string-text-to-slug (text)
  "Turn TEXT into a '-'-delimited slug suitable for use in a URL."
  (let (;; Combining Diacritical Marks
        ;; https://www.unicode.org/charts/PDF/U0300.pdf
        (slug-trim-chars '(768  ; U+0300 COMBINING GRAVE ACCENT
                           769  ; U+0301 COMBINING ACUTE ACCENT
                           770  ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771  ; U+0303 COMBINING TILDE
                           772  ; U+0304 COMBINING MACRON
                           774  ; U+0306 COMBINING BREVE
                           775  ; U+0307 COMBINING DOT ABOVE
                           776  ; U+0308 COMBINING DIAERESIS
                           777  ; U+0309 COMBINING HOOK ABOVE
                           778  ; U+030A COMBINING RING ABOVE
                           779  ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780  ; U+030C COMBINING CARON
                           795  ; U+031B COMBINING HORN
                           803  ; U+0323 COMBINING DOT BELOW
                           804  ; U+0324 COMBINING DIAERESIS BELOW
                           805  ; U+0325 COMBINING RING BELOW
                           807  ; U+0327 COMBINING CEDILLA
                           813  ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814  ; U+032E COMBINING BREVE BELOW
                           816  ; U+0330 COMBINING TILDE BELOW
                           817))) ; U+0331 COMBINING MACRON BELOW
    (cl-flet* ((nonspacing-mark-p (char)
                 (memq char slug-trim-chars))
               (strip-nonspacing-marks (s)
                 (string-glyph-compose
                  (apply #'string
                         (seq-remove #'nonspacing-mark-p
                                     (string-glyph-decompose s)))))
               (cl-replace (text pair)
                 (replace-regexp-in-string (car pair) (cdr pair) text)))
      (let* ((pairs `(;; convert anything not alphanumeric
                      ("[^[:alnum:][:digit:]]" . "-")

                      ("--*" . "-") ; remove sequential underscores
                      ("^-" . "")   ; remove starting underscore
                      ("-$" . ""))) ; remove ending underscore
             (slug (-reduce-from #'cl-replace
                                 (strip-nonspacing-marks text)
                                 pairs)))
        (downcase slug)))))

(provide 'ok-string)
;;; ok-string.el ends here
