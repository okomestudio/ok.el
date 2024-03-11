;;; ok-web.el --- Okome Studio web utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun ok-web-hatena-visit-bookmark-comments (arg &optional url)
  "Visit the Hatena Bookmark comments page for the URL.
If URL is not supplied, the function will attempt to yank one
from kill ring based on ARG. The page will be opened with eww or
with an external browser if prefixed."
  (interactive "P")
  (let* ((url (if (equal url nil) (current-kill 0) url))
         (hatena-url (string-replace "https://"
                                     "https://b.hatena.ne.jp/entry/s/"
                                     url)))
    (ok-web-url-visit arg hatena-url)))

(defun ok-web-url-visit (arg url)
  "Visit URL with eww or an external browser if prefixed (with ARG)."
  (interactive "P")
  (pcase arg
    ('(4) (browse-url url))
    (_ (eww url))))

(provide 'ok-web)
;;; ok-web.el ends here
