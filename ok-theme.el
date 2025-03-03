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

(defcustom ok-theme-default nil
  "Default theme."
  :type 'symbol
  :group 'ok)

(defun ok-theme-prepare-enable-on-startup (&optional theme)
  "Prepare THEME to be enabled at Emacs startup.
THEME defaults to `ok-theme-default' if not given,"
  (let ((theme (or theme ok-theme-default)))
    (if (daemonp)
        (progn
          (defun ok-theme-enable--as-daemon (frame)
            (with-selected-frame frame
              (ok-theme-enable theme)))

          (add-hook 'after-make-frame-functions #'ok-theme-enable--as-daemon))

      (defun ok-theme-enable--after-init ()
        (ok-theme-enable theme))

      (add-hook 'after-init-hook #'ok-theme-enable--after-init))))

(defun ok-theme-enable (theme)
  "Disable all enabled themes before `enable-theme' FUN on THEME.
NOTE(2025-03-03): It is important to use `load-theme' with the enable
option to enable a theme non-interactively. `enable-theme' does not work
when used non-interactively."
  (interactive
   (list (intern (completing-read
                  "Enable custom theme: "
                  obarray (lambda (sym)
                            (get sym 'theme-settings)) t))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(provide 'ok-theme)
;;; ok-theme.el ends here
