;;; ok-datetime.el --- Date & Time  -*- lexical-binding: t -*-
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

(defun ok-datetime-parse-human-readable (s)
  "Given a human-readable date string S, return datetime as a list.
The returned list is (sec min hr day mon yr dow dst tz). This
function uses the date shell command."
  (parse-time-string
   (with-temp-buffer
     (call-process "env" nil t nil "LC_ALL=C" "LANGUAGE=" "date" "-d" s)
     ;; (or (bobp) (delete-backward-char 1))
     (or (bobp) (delete-char -1))
     (buffer-string))))

(provide 'ok-datetime)
;;; ok-datetime.el ends here
