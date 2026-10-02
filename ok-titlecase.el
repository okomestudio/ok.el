;;; ok-titlecase.el --- titlecase Extension  -*- lexical-binding: t -*-
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
;; Provides extras for the `titlecase' package
;; (https://codeberg.org/acdw/titlecase.el).
;;
;;; Code:

;; Only uses a macro, so this needs to run only when byte-complied:
(eval-when-compile (require 'cl-lib))

(require 'titlecase)

(defun ok-titlecase-dwim (&optional style interactivep)
  "Title-case the region or current line.
This is a drop-in, less jumpy version of `titlecase-dwim'. Keeping the
point where it should be, as `titlecase-dwim doesn't take care of it."
  (interactive "i\nP")
  (let* ((use-reg (use-region-p))
         (beg (if use-reg (region-beginning) (line-beginning-position)))
         (offset (- (point) beg)))
    (titlecase-dwim style interactivep)
    (goto-char (min (+ beg offset)
                    (if use-reg (region-end) (line-end-position))))))

(defun ok-titlecase--headline (text)
  "Normalize headline TEXT, taking into account prefix like Chapter/Section."
  (let* ((case-fold-search t)
         (num "[0-9]+\\|[一二三四五六七八九十〇]+")
         (num-words '("i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix" "x"
                      "one" "two" "three" "four" "five"
                      "six" "seven" "eight" "nine" "ten"
                      "eleven" "twelve" "thirteen" "fourteen" "fifteen"
                      "sixteen" "seventeen" "eighteen" "nineteen" "twenty"))
         (num-alpha '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"))

         (re-num (concat "\\(?:" num "\\|" (regexp-opt num-words) "\\)"))
         (re-alnum (concat "\\(?:" num "\\|" (regexp-opt (append num-words num-alpha)) "\\)"))

         (re-delim "[.: \t─—–-]")
         (re (concat
              "^\\(?:"
              "\\(?1:\\(" re-num "\\)\\([.:─—–-]\\)?[ \t]+\\)"
              "\\|"
              "\\(?4:\\(chap\\(?:ter\\)?\\|ch\\|part\\|appendix\\)\\.?[ \t]+\\(" re-alnum "\\)" re-delim "*\\)"
              "\\|"
              "\\(?7:\\(第\\)\\(" re-num "\\)\\(章\\)[ \t]*\\)"
              "\\)\\(?11:.*\\)$"))
         prefix title)
    (if-let* ((_ (string-match re text))
              (s (match-string 11 text)))
        (progn
          (setq title (string-trim s))
          (cond
           ((match-string 1 text)
            (let* ((num (match-string 2 text))
                   (delim (match-string 3 text)))
              (setq prefix (when num (concat num (if delim delim ".") " ")))))
           ((match-string 4 text)
            (let* ((raw-prefix (match-string 5 text))
                   (num (match-string 6 text))
                   (normed (when raw-prefix
                             (titlecase--string raw-prefix titlecase-style))))
              (setq prefix (format "%s%s. "
                                   (if normed (concat normed " ") "")
                                   num))))
           ((match-string 7 text)
            (let* ((raw-prefix-1 (match-string 8 text))
                   (num (match-string 9 text))
                   (raw-prefix-2 (match-string 10 text)))
              (setq prefix (when num
                             (concat (if raw-prefix-1 raw-prefix-1 "")
                                     num
                                     (if raw-prefix-2 raw-prefix-2 "")
                                     " ")))))))
      (setq title (string-trim text)))
    (concat (if prefix prefix "")
            (titlecase--string title titlecase-style))))

(defun ok-titlecase-headlines ()
  "Iterate over headlines in the region or buffer, prompting to titlecase them.
Matches Org-mode (e.g., '* Headline') and Markdown (e.g., '# Headline') formats."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only supports Org mode buffers"))
  (let ((changes-made 0)
        (scope (if (use-region-p) 'region nil)))
    (org-map-entries
     (lambda ()
       (let* ((orig-title (org-get-heading t t t t)) ; work on just headline texts
              (titlecased (ok-titlecase--headline orig-title)))
         (when (and (not (string-empty-p orig-title))
                    (not (string= orig-title titlecased)))
           (save-excursion
             (beginning-of-line)
             (when (re-search-forward (regexp-quote orig-title) (line-end-position) t)
               (let ((t-start (match-beginning 0))
                     (t-end (match-end 0))
                     (ov (make-overlay (match-beginning 0) (match-end 0))))
                 (overlay-put ov 'face 'highlight)
                 (unwind-protect
                     (when (y-or-n-p (format "Change: '%s' -> '%s'? "
                                             orig-title titlecased))
                       (goto-char t-start)
                       (delete-region t-start t-end)
                       (insert titlecased)
                       (cl-incf changes-made))
                   (delete-overlay ov))))))))
     t scope)
    (message "Titlecased %d headline%s." changes-made (if (= changes-made 1) "" "s"))))

(provide 'ok-titlecase)
;;; ok-titlecase.el ends here
