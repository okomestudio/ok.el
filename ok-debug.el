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

(defcustom ok-debug nil
  "Global flag for debugging.
Set to non-nil to be in debug mode. Set to nil to non-debug mode."
  :group 'ok)

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

(defun ok-debug-prepend-timestamp (fun format-string &rest args)
  "Prepend timestamp to `message' output.
FORMAT-STRING and ARGS are passed through to FUN. See
emacs.stackexchange.com/a/38511/599."
  ;; If `message-log-max' is nil, message logging is disabled
  (when message-log-max
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (when (not (bolp)) (newline))
        (insert (format-time-string "%FT%H:%M:%S.%3N" (current-time)) " ")
        (apply fun `(,format-string ,@args))
        (when ok-debug
          ;; NOTE: Temporarily add args to inspect the source of empty
          ;; lines in *Message* buffer:
          (backward-char)
          (insert (format " [\"%s\" %s]" format-string `(,args)) ""))))))

(advice-add #'message :around #'ok-debug-prepend-timestamp)

(provide 'ok-debug)
;;; ok-debug.el ends here
