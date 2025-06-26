;;; ok-help.el --- Enhanced help.el  -*- lexical-binding: t -*-
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
;;
;; The module to enhance built-in `help.el'.
;;
;;; Code:

(require 'ansi-color)
(require 'ok)

(defvar ok-help-shell-cmd-opts
  '((curl . (("--help" "all"))))
  "Command to help options.")

;;;###autoload
(defun ok-help-shell-cmd (cmd)
  "Show the shell CMD help in a special buffer."
  (interactive (list (ok-prompt-or-string-from-region "Shell command: ")))
  (if (not (executable-find cmd))
      (warn "Command not found: %s" cmd)
    (let ((buffer-stdout "*shell-help*")
          (buffer-stderr "*shell-help-error*")
          (postfilter (when (executable-find "batcat")
                        '("|" "batcat" "-f" "-l" "help" "-p" "--theme=ansi")))
          status)
      (when (catch 'success
              (dolist (opts (or (alist-get (intern cmd) ok-help-shell-cmd-opts)
                                '(("--help") ("-help") ("-h"))))
                (setq status
                      (shell-command (string-join `(,cmd ,@opts ,@postfilter)
                                                  " ")
                                     buffer-stdout
                                     buffer-stderr))
                (if (= status 0)
                    (throw 'success t)
                  (with-current-buffer buffer-stdout
                    (erase-buffer)
                    (setq buffer-read-only t))
                  (switch-to-buffer buffer-stdout))))
        (with-current-buffer buffer-stdout
          (ansi-color-apply-on-region (point-min) (point-max))
          (setq buffer-read-only t)
          (local-set-key (kbd "q") #'quit-window))
        (switch-to-buffer buffer-stdout)))))

(provide 'ok-help)
;;; ok-help.el ends here
