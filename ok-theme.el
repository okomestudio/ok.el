;;; ok-theme.el --- Theme Enhancements  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2024-2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The module for enhancing custom theme-related functions.
;;
;;; Code:

(require 'dash)

(defcustom ok-theme-custom-themes nil
  "Custom themes to make available for lazy loading.
This is a list of cons `(feature . file)'. `feature' as a symbol is the
feature providing themes. `file' is a file path (either absolute or
relative to `user-emacs-directory') to Lisp file configuring the themes."
  :type '(repeat (cons symbol string))
  :group 'ok)

(defcustom ok-theme-presets nil
  "The alist defining theme presets.
In each alist element, the key is a theme preset name (a symbol). The
value is an ordered list of `(theme . feature)' pairs. When a theme
preset is activated, these pairs are loaded and enabled in that order to
ensure deterministic theme loading behavior."
  :type '(repeat (cons symbol (repeat (cons symbol symbol))))
  :group 'ok)

;;;###autoload
(defun ok-theme-enable-preset (preset)
  "Load and enable a PRESET in `ok-theme-presets'."
  (interactive (list (intern (completing-read "Enable preset themes: "
                                              (--map (car it) ok-theme-presets)))))
  (let ((themes (alist-get preset ok-theme-presets)))
    (dolist (feature (--map (cdr it) themes))
      (ok-theme-prepare feature))
    (ok-theme-enable (--map (car it) themes))))

;;;###autoload
(defun ok-theme-prepare-enable-on-startup (preset)
  "Prepare a PRESET in `ok-theme-presets' for activation at Emacs startup."
  (if (daemonp)
      (progn
        (defun ok-theme-enable--as-daemon (frame)
          (with-selected-frame frame
            (ok-theme-enable-preset preset)))

        (add-hook 'after-make-frame-functions #'ok-theme-enable--as-daemon))

    (defun ok-theme-enable--after-init ()
      (ok-theme-enable-preset preset))

    ;; NOTE(2025-09-16): Theme activation should be done before buffers are
    ;; created, as some changes happen with mode hooks. Not enabling early can
    ;; cause buffers loaded by desktop to be not fully themed.
    (add-hook 'after-init-hook #'ok-theme-enable--after-init -98)))

(defvar before-enable-theme-functions nil
  "Hooks to run just before the function `ok-theme-enable' enables a theme.
The hook function is called with `theme' as an argument.")

;;;###autoload
(defun ok-theme-enable (theme)
  "Enable THEME after disabling all themes.
THEME is either a symbol or a list of symbols specifying themes."
  (interactive
   (list (intern (completing-read "Enable custom theme: "
                                  obarray (lambda (sym)
                                            (get sym 'theme-settings)) t))))
  (mapc #'disable-theme custom-enabled-themes)

  (dolist (theme (if (listp theme) theme `(,theme)))
    (run-hook-with-args 'before-enable-theme-functions theme)

    ;; NOTE(2025-03-03): It is important to use `load-theme' with the
    ;; enable option to enable a theme non-interactively.
    ;; `enable-theme' does not work when used non-interactively.
    (load-theme theme t)))

(defun ok-theme-prepare (feature)
  "Load FEATURE to make its themes available."
  (interactive
   (list (intern (completing-read "Prepare custom theme: "
                                  (--filter (car it)
                                            ok-theme-custom-themes)))))
  (if-let* ((file (alist-get feature ok-theme-custom-themes)))
      (progn
        (setq file (or (and (file-name-absolute-p file) file)
                       (ok-file-expand-user-emacs-file file)))
        (condition-case nil
            (load file)
          (error "Could not load theme config file (%s) for `%s'"
                 file feature)))
    (error "Feature `%s' not defined in `ok-theme-custom-themes'" feature))
  (require feature))

(provide 'ok-theme)
;;; ok-theme.el ends here
