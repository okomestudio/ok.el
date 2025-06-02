;;; ok-file.el --- Okome Studio file utilities  -*- lexical-binding: t -*-
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

(defun ok-file-ensure-directory-exists (path)
  "Ensure the directory exists at PATH."
  (if (not (file-directory-p path))
      (make-directory path :parents)))

(defun ok-file-ensure-from-github (src &optional dest)
  "Ensure that a file hosted by GitHub gets downloaded and exists.
SRC is the source path in GitHub, DEST is the local destination
path for the downloaded file. See okutil-ensure-file-from-url for
detail."
  (ok-file-ensure-from-url
   (concat "https://raw.githubusercontent.com/" src) dest))

(defun ok-file-ensure-from-url (src &optional dest)
  "Download file from SRC and save as the local file DEST.

SRC is the source URL. If DEST is not given, the filename is
inferred from the URL and used as relative DEST. Relative DEST
will be resolved relative to `default-directory'. If DEST points
to an existing directory, a file will be created with the same
name as in SRC.

When a new file is created, the function returns the path to it."
  (let* ((filename (file-name-nondirectory src))
         (dest (if (and dest (file-name-absolute-p dest))
                   dest
                 (expand-file-name (or dest filename)))))
    (when (file-directory-p dest)
      (setq dest (file-name-concat dest filename)))
    (when (not (file-exists-p dest))
      (url-copy-file src dest)
      dest)))

(defun ok-file-expand-user-emacs-file (&rest components)
  "Expand the path by concatenating COMPONENTS to `user-emacs-directory'."
  (convert-standard-filename
   (locate-user-emacs-file (apply #'file-name-concat components))))

(defun ok-file-locate-dominating-files (file name)
  "Look upward from FILE in directory tree to locate files named NAME.
This extends the `locate-dominating-file' function such that the search
does not stop at the first occurrence of NAME and continues
looking upward in the directory tree.

The function returns a list. The list elements are sorted  such that
subdirectories come toward the end."
  (let ((dominating-file (locate-dominating-file file name))
        dir-locals-files parent-dir)
    (while dominating-file
      (push dominating-file dir-locals-files)
      (setq parent-dir (ok-file-parent-directory dominating-file))
      (setq dominating-file (and (not (null parent-dir))
                                 (locate-dominating-file parent-dir name))))
    dir-locals-files))

(defun ok-file-parent-directory (path)
  "Get the parent directory of PATH.
This function effectively removes the last component from PATH,
regardless of the path pointing to a file or a directory. The returned
directory always ends with a '/' character. The function returns nil
if no parent directory exists (i.e., PATH points to root)."
  (setq path (string-trim path))
  (unless (or (string= "/" path) (string-empty-p path))
    (file-name-directory (directory-file-name (expand-file-name path)))))

(provide 'ok-file)
;;; ok-file.el ends here
