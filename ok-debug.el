;;; ok-debug.el --- Okome Studio debug utilities  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
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
;; The `ok-debug' module provides debug utilities.
;;
;;; Code:

(defvar ok-debug--variables nil
  "Variables to have the same value as `ok-debug'.")

(defcustom ok-debug nil
  "Global flag for debugging.
Set to non-nil for debug mode. Set to nil for non-debug mode."
  :group 'ok
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (mapc (lambda (it) (ok-debug--sync-var it val)) ok-debug--variables)))

(defun ok-debug ()
  "Toggle `ok-debug'."
  (interactive)
  (setopt ok-debug (not ok-debug)))

(defun ok-debug--sync-var (var val)
  "Set the value to VAR based on VAL."
  (cond ((symbolp var)
         (eval `(setopt ,var ,val)))
        ((consp var)
         (eval `(setopt ,(car var) ',(if val (cadr var) (cddr var)))))))

(defun ok-debug-register (&rest vars)
  "Register variables in the list VARS to sync with `ok-debug'.
Each element of VARS is either a symbol or a cons cell of form `(var . (t-val .
nil-val))', where `var' is set to `t-val' when `ok-debug' is non-nil and to
`nil-val' when it is nil."
  (dolist (var vars)
    (ok-debug--sync-var var ok-debug)
    (add-to-list 'ok-debug--variables var)))

(defun ok-debug--fun-notify-entry (fun &rest args)
  "Wrap (FUN ARGS) in an around advice to log entry/exit."
  (let (result)
    (message "BEG %s %s" fun args)
    (setq result (apply fun args))
    (message "END %s" fun)
    result))

(defun ok-debug-fun-notify-entry (&rest funs)
  "Notify entry/exist of each function in FUNS."
  (dolist (fun funs)
    (advice-add fun :around #'ok-debug--fun-notify-entry)))

(defun ok-debug-ad-function-beg-end (old-fun &rest _)
  "Advice OLD-FUN to message at the beginning and end of execution."
  (let ((old-fun-sym (nth 2 (nth 0 (reverse (ok-debug-call-stack)))))
        (t0 (float-time))
        result)
    (message "BEG %s" old-fun-sym)
    (setq result (apply old-fun _))
    (message "END %s (ran for %f sec)" old-fun-sym (- (float-time) t0))
    result))

(defun ok-debug-call-stack ()
  "Return the current call stack frames.
See https://emacs.stackexchange.com/a/2312/599."
  (let ((index 5) frame frames)
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-remove-if-not 'car frames)))

(defun ok-debug-caller (&optional n)
  "Return the N-th caller in the stack frames.
N is a zero-index, starting from current frame."
  (nth 1 (nth (or n 0) (cl-rest (reverse (ok-debug-call-stack))))))

(defun ok-debug-execution-time (name &rest body)
  "Measure execution time of BODY named NAME."
  (let ((t0 (float-time)))
    (unwind-protect
        (progn body)
      (message "%s ran in %f" name (- (float-time) t0)))))

(defun ok-debug-message (text)
  "Print a debug message TEXT with timestamp."
  (message "%s %s"
           (format-time-string "%FT%H:%M:%S.%3N" (current-time))
           text))

(defcustom ok-debug-message-args nil
  "Global flag for debugging.
Set to non-nil to show arguments to `message' when `ok-debug' is non-nil."
  :group 'ok
  :type 'boolean)

(defun ok-debug-prepend-timestamp (fun format-string &rest args)
  "Prepend timestamp to `message' output.
FORMAT-STRING and ARGS are passed through to FUN (`message')."
  ;; Be mindful of the role of `message-log-max' in `message'. When it
  ;; is non-nil, the output is sent to both minibuffer and the
  ;; *Message* buffer. When it is nil, the output is sent to
  ;; minibuffer only.
  (let ((deactivate-mark nil)
        (inhibit-read-only t))
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (when message-log-max
        (when (not (bolp)) (newline))
        (insert (format-time-string "%FT%H:%M:%S.%3N" (current-time)) " "))
      (apply fun `(,format-string ,@args))
      (when (and message-log-max ok-debug ok-debug-message-args)
        (backward-char)
        (insert (format " [\"%s\" %s]" format-string `(,args)) "")))))

(advice-add #'message :around #'ok-debug-prepend-timestamp)

(provide 'ok-debug)
;;; ok-debug.el ends here
