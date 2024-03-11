;;; ok-prompt.el --- Okome Studio prompt utilities  -*- lexical-binding: t -*-
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
