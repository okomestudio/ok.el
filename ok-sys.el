;;; ok-sys.el --- Okome Studio system utilities  -*- lexical-binding: t -*-
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

(defun ok-sys-monitor-count ()
  "Get the number of display monitors."
  (length (display-monitor-attributes-list)))

(defun ok-sys-linux-p ()
  "Return non-nil if system is Linux-based."
  (string-equal system-type "gnu/linux"))

(defun ok-sys-macos-p ()
  "Return non-nil if system is Mac OS X-based."
  (string-equal system-type "darwin"))

(provide 'ok-sys)
;;; ok-sys.el ends here
