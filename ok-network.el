;;; ok-network.el --- ok-network  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun ok-network-port-open-async (host port callback)
  "Check asynchronously if HOST and PORT are open.
Invokes CALLBACK with two arguments, the first being alive status,
t (up) or nil (down), and the second being the full event message."
  (make-network-process
   :name "async-port-check"
   :host host
   :service port
   :type nil                  ; stream
   :nowait t
   :sentinel (lambda (proc event)
               (let ((alive (string-prefix-p "open" event)))
                 (when (process-live-p proc)
                   (delete-process proc))
                 (funcall callback alive event)))))

(provide 'ok-network)
;;; ok-network.el ends here
