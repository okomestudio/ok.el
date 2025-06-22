;;; ok.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ok.el
;; Version: 0.5.4
;; Keywords: development, convenience
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))
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
;; Okome Studio utilities.
;;
;;; Code:

(require 'ok-buffer)
(require 'ok-cext)
(require 'ok-datetime)
(require 'ok-debug)
(require 'ok-desktop)
(require 'ok-dir-locals)
(require 'ok-edit)
(require 'ok-face)
(require 'ok-file)
(require 'ok-font)
(require 'ok-minibuffer)
(require 'ok-prompt)
(require 'ok-string)
(require 'ok-sys)
(require 'ok-theme)
(require 'ok-web)

(defgroup ok nil
  "Okome Studio utility group."
  :prefix "ok-"
  :group 'development)

(defmacro ok-safe-local-variable-add (&rest pairs)
  "Add variable-function PAIRS to `safe-local-variable'."
  (unless (zerop (mod (length pairs) 2))
    (error "PAIRS must have an even number of variable-value pairs"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Not a symbol: %s" (car pairs)))
      (setq expr
            (cons (list 'put
                        (list 'quote (car pairs))
                        (list 'quote 'safe-local-variable)
                        (list 'function (car (cdr pairs))))
                  expr))
      (setq pairs (cdr (cdr pairs))))
    (macroexp-progn (nreverse expr))))

(defun ok-eval-form-recursively (form)
  "Recursively evaluate a Lisp FORM.
This function handles backquote comma forms, backquotes, and nested
structures and returns the fully evaluated form."
  (cond
   ((atom form) form)       ; atomic forms
   ((consp form)            ; cons cells
    (pcase form
      ;; Comma form: (\, EXPR) -> evaluate EXPR
      ((and `(,\ ,expr) (guard (eq (car form) '\,)))
       (condition-case err
           (eval (ok-eval-form-recursively expr) t)
         (error (message "Error evaluating comma form %s: %s" expr err) nil)))

      ;; Backquote form: (backquote FORM) -> return FORM
      (`(backquote ,value)
       (ok-eval-form-recursively value))

      ;; Other lists: recurse on each element
      (_ (mapcar #'ok-eval-form-recursively form))))
   (t (error "Invalid form: %s" form))))

(provide 'ok)
;;; ok.el ends here
