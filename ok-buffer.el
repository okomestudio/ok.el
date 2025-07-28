;;; ok-buffer.el --- Enhance buffer functions  -*- lexical-binding: t -*-
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
;; This module enhances builtin buffer functions.
;;
;;; Code:

(defun ok-buffer-kill-all-but-special (&optional buffers)
  "Kill all buffers that are neither special or internal.
Special or internal buffers are identified by their name wrapped with '*'
characters, e.g., '*Messages*'.

The currently selected buffer will not be killed. When given, buffers in the
list BUFFERS are also excluded from killing."
  (interactive)
  (let* ((buffers (when (and buffers (not (listp buffers)))
                    (list buffers)))
         (buffers (append buffers (list (current-buffer)))))
    (message "KKK")
    (pp buffers)
    (mapc (lambda (buffer)
            (unless (or (string-match-p "\\*.*\\*" (buffer-name buffer))
                        (member buffer buffers))
              (kill-buffer buffer)))
          (buffer-list))))

(defun ok-buffer-kill-others ()
  "Kill all other buffers.
See https://stackoverflow.com/a/3417473/515392."
  (interactive)
  ;; Kill buffers visiting files unless it's current.
  (mapc #'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not #'buffer-file-name (buffer-list))))
  ;; Remove other buffers explicitly.
  (mapc #'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not
               (lambda (buffer)
                 (string-match "^\\(\\*helpful \\|null\\|magit[-:]\\)"
                               (buffer-name buffer)))
               (buffer-list)))))

(defun ok-buffer-revert-no-confirm (&optional force-reverting)
  "Interactive call to `revert-buffer'.
Ignore auto-save and do not request for confirmation. When the current buffer is
modified, the command refuses to revert it, unless you specify the optional
argument: FORCE-REVERTING to non-nil."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(provide 'ok-buffer)
;;; ok-buffer.el ends here
