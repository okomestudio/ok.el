;;; ok.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/
;; Version: 0.1
;; Keywords: development, convenience
;; Package-Requires: ((emacs "29.1"))
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
(require 'ok-datetime)
(require 'ok-edit)
(require 'ok-face)
(require 'ok-font)
(require 'ok-file)
(require 'ok-prompt)
(require 'ok-string)
(require 'ok-sys)
(require 'ok-web)

(defgroup ok nil
  "Okome Studio utility group."
  :prefix "ok-"
  :group 'development)

(defcustom ok-debug nil
  "Global flag for debugging.
Set to non-nil to be in debug mode. Set to nil to non-debug mode."
  :group 'ok)

(defun ok-debug-message (text)
  "Print a debug message TEXT with timestamp."
  (message "%s %s"
           (format-time-string "%FT%H:%M:%S.%3N" (current-time))
           text))

(defun ok-prepend-timestamp (format-string &rest args)
  "Prepend timestamp to `message' output.
FORMAT-STRING and ARGS are passed through. See
https://emacs.stackexchange.com/a/38511/599."
  (unless (string-equal format-string "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (insert (format-time-string "%FT%H:%M:%S.%3N" (current-time)) " "
                ;; NOTE: Temporarily added to find the source of empty
                ;; lines in *Message* buffer
                (if ok-debug (format "[\"%s\" %s] " format-string `(,args)) ""))))))

(advice-add 'message :before 'ok-prepend-timestamp)

(defun ok-execution-time (name &rest body)
  "Measure execution time of BODY named NAME."
  (let ((t0 (float-time)))
    (unwind-protect
        (progn body)
      (message "%s ran in %f" name (- (float-time) t0)))))

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

(provide 'ok)
;;; ok.el ends here
