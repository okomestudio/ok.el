;;; ok-font.el --- Font  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun ok-font-family-exists-p (font-family)
  "Return non-nil if font with FONT-FAMILY exists."
  (if (find-font (font-spec :family font-family))
      t
    (message "WARNING: Font for `%s' not found" font-family)))

(defun ok-font-installed-p (font-name)
  "Return non-nil if font with FONT-NAME is installed."
  ;; In X, `x-list-fonts' was used
  (find-font (font-spec :name font-name)))

(provide 'ok-font)
;;; ok-font.el ends here
