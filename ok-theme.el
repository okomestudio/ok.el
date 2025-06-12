;;; ok-theme.el --- Okome Studio system utilities  -*- lexical-binding: t -*-
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

(require 'dash)

(defcustom ok-theme-custom-themes nil
  "Themes to make available for lazy loading.
Each item in the list is `(feature . file)'. `file' is either an
absolute path or a path relative to `user-emacs-directory'."
  :type '(repeat (cons symbol string))
  :group 'ok)

;;;###autoload
(defun ok-theme-prepare-enable-on-startup (theme feature)
  "Prepare THEME defined in FEATURE to be enabled at Emacs startup."
  (if (daemonp)
      (progn
        (defun ok-theme-enable--as-daemon (frame)
          (with-selected-frame frame
            (ok-theme-prepare feature)
            (ok-theme-enable theme)))

        (add-hook 'after-make-frame-functions #'ok-theme-enable--as-daemon))

    (defun ok-theme-enable--after-init ()
      (ok-theme-prepare feature)
      (ok-theme-enable theme))

    ;; Espeically when `desktop-save-mode' is active, theme activation
    ;; should be performed early, so that they are ready when saved
    ;; desktop settings are restored.
    (add-hook 'after-init-hook #'ok-theme-enable--after-init -99)))

(defvar before-enable-theme-functions nil
  "Hooks to run just before enabling a theme in `ok-theme-enable'.
The hook should take `theme' as an argument.")

(defun ok-theme-enable (theme)
  "Enable THEME after disabling all other themes."
  (interactive
   (list (intern (completing-read
                  "Enable custom theme: "
                  obarray (lambda (sym)
                            (get sym 'theme-settings)) t))))
  (mapc #'disable-theme custom-enabled-themes)
  (run-hook-with-args 'before-enable-theme-functions theme)

  ;; NOTE(2025-03-03): It is important to use `load-theme' with the
  ;; enable option to enable a theme non-interactively. `enable-theme'
  ;; does not work when used non-interactively.
  (load-theme theme t))

(defun ok-theme-prepare (feature)
  "Load FEATURE to make available the themes it defines."
  (interactive
   (list (intern (completing-read "Prepare custom theme: "
                                  (--filter (car it)
                                            ok-theme-custom-themes)))))
  (let* ((file (alist-get feature ok-theme-custom-themes))
         (file (pcase (file-name-absolute-p file)
                 ('t file)
                 (_ (ok-file-expand-user-emacs-file file)))))
    (condition-case nil
        (load file)
      (error "File (%s) not found for theme feature `%s'" file feature)))
  (require feature))

(provide 'ok-theme)
;;; ok-theme.el ends here
