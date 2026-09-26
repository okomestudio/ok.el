;;; ok-japan-util.el --- japan-util Extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2026 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Provides extras for the built-in `japan-util' package.
;;
;;; Code:

(require 'japan-util)

(defcustom ok-japan-util-hankaku-exclude-chars
  "！？（）｛｝［］〈〉：；／"
  "Characters to exclude from Zenkaku to Hankaku conversion.
Can be a string (e.g. \"！％（）\") or a list of character codes."
  :type '(choice (string :tag "String of characters")
                 (repeat :tag "List of characters" character))
  :group 'ok)

(defun ok-japan-util-hankaku (str &optional exclude-chars)
  "Convert Zenkaku characters in STR to Hankaku while preserving text properties.
EXCLUDE-CHARS can be a string or a list of characters.
Optional arguments ASCII-ONLY and KATAKANA-ONLY restrict conversion."
  (let* ((excludes (or exclude-chars ok-japan-util-hankaku-exclude-chars))
         (exclude-list (cond
                        ((stringp excludes) (append excludes nil))
                        ((listp excludes) excludes)
                        (t nil)))
         (len (length str))
         (i 0)
         (chunks nil))
    (while (< i len)
      (let* ((ch (aref str i))
             (props (text-properties-at i str))
             (converted (if (memq ch exclude-list)
                            (char-to-string ch)
                          (let ((res (japanese-hankaku ch t)))
                            (if (characterp res)
                                (char-to-string res)
                              res))))
             ;; Create a copy so we don't mutate shared string constants
             (chunk (copy-sequence converted)))
        ;; Copy original character's text properties onto the converted chunk
        (when props
          (add-text-properties 0 (length chunk) props chunk))
        (push chunk chunks)
        (setq i (1+ i))))
    (apply #'concat (nreverse chunks))))

(defun ok-japan-util-norm (beg end &optional exclude-chars)
  "Convert Zenkaku characters to Hankaku in region from START to END.
Preserves text properties (styling, faces, etc.).
Excludes characters specified in EXCLUDE-CHARS (or `my-japanese-hankaku-exclude-chars`)."
  (interactive "r")
  (let* ((orig-text (buffer-substring beg end))
         (new-text (ok-japan-util-hankaku orig-text exclude-chars)))
    (delete-region beg end)
    (insert new-text)))

(provide 'ok-japan-util)
;;; ok-japan-util.el ends here
