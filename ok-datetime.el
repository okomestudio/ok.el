;;; ok-datetime.el --- Date & Time  -*- lexical-binding: t -*-
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
