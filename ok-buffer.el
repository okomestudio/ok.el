;;; ok-buffer.el --- Okome Studio buffer utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun ok-buffer-kill-others ()
  "Kill all other buffers.
See https://stackoverflow.com/a/3417473/515392."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

(defun ok-buffer-revert-no-confirm (&optional force-reverting)
  "Interactive call to `revert-buffer'.
Ignore auto-save and do not request for confirmation. When the
current buffer is modified, the command refuses to revert it,
unless you specify the optional argument: FORCE-REVERTING to
non-nil."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(provide 'ok-buffer)
;;; ok-buffer.el ends here
