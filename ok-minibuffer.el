;;; ok-minibuffer.el --- Okome Studio minibuffer utilities  -*- lexical-binding: t -*-
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
;;; Code:

(defun ok-minibuffer-kill-all-but-one ()
  "Delete all minibuffers but one."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and (string-match "\*Minibuf-[0-9]+\*" name)
                 (not (string= name " *Minibuf-0*")))
        (let ((buf (get-buffer name)))
          (with-current-buffer name
            (set-buffer-modified-p nil)
            (kill-buffer  buf)
            (message "Closed %s" name)))))))

(provide 'ok-minibuffer)
;;; ok-minibuffer.el ends here
