;;; ok.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Okome Studio utilities.
;;
;;; Code:

(defun ok-kill-other-buffers ()
  "Kill all other buffers.
See https://stackoverflow.com/a/3417473/515392."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(provide 'ok)
;;; ok.el ends here
