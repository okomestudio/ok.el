;;; ok-dir-locals.el --- Okome Studio directory locals utilities  -*- lexical-binding: t -*-
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
;; The `ok-dir-locals' module provides directory locals utilities.
;;
;;; Code:

(defun ok-dir-locals-visit ()
  "Open the nearest .dir-locals.el file for the current buffer."
  (interactive)
  (let ((dir (locate-dominating-file default-directory ".dir-locals.el")))
    (if dir
        (find-file (expand-file-name ".dir-locals.el" dir))
      (message "No .dir-locals.el file found in the directory tree"))))

(provide 'ok-dir-locals)
;;; ok-dir-locals.el ends here
