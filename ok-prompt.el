;;; ok-prompt.el --- Okome Studio prompt utilities  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024 Taro Sato
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

(defun ok-prompt-or-string-from-region (prompt &optional initial history default inherit)
  "Read string from region, at-point, or PROMPT.
See `read-string` for the meaning of INITIAL, HISTORY, DEFAULT, and INHERIT."
  (if (region-active-p)
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))
    (let ((word (thing-at-point 'word 'no-properties)))
      (if word
          word
        (read-string prompt initial history default inherit)))))

(provide 'ok-prompt)
;;; ok-prompt.el ends here
