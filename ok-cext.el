;;; ok-cext.el --- Okome Studio C extension  -*- lexical-binding: t -*-
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
;; This library extends Emacs Lisp features supplied through C.
;;
;;; Code:

(defvar ok-upcase-word--counter -1
  "Word counter for `ok-upcase-word'.")

(defun ok-upcase-word (&optional _)
  "Extended `upcase-word'.
When a point is at the end of word, this function applies upcase to the
previous word. The repeat application of this function leads
increasingly more previous word(s) to be upcased. It also marks the
upcased words an active region."
  (interactive)
  (if (and (eq last-command #'ok-upcase-word)
           (looking-back "\\w\\b" 2))
      (setq ok-upcase-word--counter (1- ok-upcase-word--counter))
    (when (looking-back "\\w\\b" 2)
      (setq ok-upcase-word--counter -1)))
  (when (looking-back "\\w\\b" 2)
    (let ((start-pos (point))
          (word-count (abs ok-upcase-word--counter)))
      (backward-word word-count)
      (let ((region-start (point)))
        (goto-char start-pos)
        (upcase-word ok-upcase-word--counter)
        (set-mark region-start)
        (goto-char start-pos)
        (activate-mark))))
  (when (region-active-p)
    (setq deactivate-mark nil)))

(provide 'ok-cext)
;;; ok-cext.el ends here
