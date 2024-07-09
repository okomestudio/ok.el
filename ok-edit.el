;;; ok-edit.el --- Okome Studio edit utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This module offers document editing utilities.
;;
;;; Code:

(defun ok-edit-fill-or-unfill-paragraph (&optional region)
  "Fill or unfill a paragraph in REGION."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fun (if (derived-mode-p 'org-mode)
                 #'org-fill-paragraph
               #'fill-paragraph)))
    (if (> (- (line-end-position) (line-beginning-position)) fill-column)
        (funcall fun nil region)
      (let* ((fill-column (point-max))
             (emacs-lisp-docstring-fill-column t))
        (funcall fun nil region)))))

(defun ok-edit-insert-newline-above ()
  "Insert a new line above current point."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (newline-and-indent) ;; do we need `indent-accoring-to-mode'?
    ))

(defun ok-edit-insert-newline-below ()
  "Insert a new line below current point."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun ok-edit-insert-zero-width-space ()
  "Insert a zero width space character at point."
  (interactive)
  (insert-char #x200b))

(defun ok-edit-sort-lines-ci ()
  "Sort lines case-insensitively."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(provide 'ok-edit)
;;; ok-edit.el ends here
