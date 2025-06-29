;;; ok-tooltip.el --- Enhanced tooltip.el  -*- lexical-binding: t -*-
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
;; The module to enhance built-in `tooltip.el'.
;;
;;; Code:

(defcustom ok-tooltip-delay 0.7
  "Seconds to wait before displaying a tooltip for the first time."
  :type 'number
  :group 'ok)

(defun ok-tooltip-display-at-point ()
  "Display a tooltip for the text at point, if available."
  (interactive)
  (when-let*
      ((point (point))
       (tooltip-text (and (not (minibufferp))
                          (bound-and-true-p tooltip-mode)
                          (get-text-property point 'help-echo)))
       (x-max-tooltip-size '(80 . 25))
       (tooltip-frame-parameters tooltip-frame-parameters)
       (window (selected-window))
       (x-y (posn-x-y (posn-at-point point window)))
       (window-x (car x-y))
       (window-y (cdr x-y))
       (frame (window-frame window))
       (frame-left (frame-parameter frame 'left))
       (frame-left (if (consp frame-left) (car frame-left) frame-left))
       (frame-top (frame-parameter frame 'top))
       (frame-top (if (consp frame-top) (car frame-top) frame-top))
       (window-origin (window-pixel-edges window))
       (window-left (nth 0 window-origin))
       (window-top (nth 1 window-origin))
       (text (if (stringp tooltip-text)
                 tooltip-text
               (funcall tooltip-text window nil point)))
       (space-width (string-pixel-width " "))
       (dx (min (length (substring-no-properties text))
                (car x-max-tooltip-size)))
       (left (+ window-x window-left frame-left
                (* space-width
                   (if (> window-x (/ (window-width window t) 2))
                       (* -1 (1- dx))
                     1))))
       (top (+ window-y window-top frame-top (* space-width 6))))
    (add-to-list 'tooltip-frame-parameters `(left . ,left))
    (add-to-list 'tooltip-frame-parameters `(top . ,top))
    (tooltip-show text)))

(defvar ok-tooltip--timer nil
  "Timer for displaying tooltips at point.")

(defun ok-tooltip--start-timer ()
  "Start a timer to display tooltips after a delay."
  (when ok-tooltip--timer
    (cancel-timer ok-tooltip--timer))
  (setq ok-tooltip--timer
        (run-with-idle-timer ok-tooltip-delay nil
                             #'ok-tooltip-display-at-point)))

(add-hook 'post-command-hook #'ok-tooltip--start-timer)

(provide 'ok-tooltip)
;;; ok-tooltip.el ends here
