;;; ok-file.el --- Okome Studio file utilities  -*- lexical-binding: t -*-
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
;; This module provides utilities generally defined in `files.el'.
;;
;;; Code:

(require 'ok-string)

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
  "Download from SRC (a URL) and save it to a file at DEST.
DEST is the destination filesystem path, defaulting to
`default-directory' if not provided. If DEST is a relative path, it gets
resolved relative to `default-directory'.

The directory component of DEST must already exists in the filesystem.
If not, the function will throw an error.

If DEST's final component is a file, the resource at SRC is renamed to
that filename upon save. Otherwise, the filename in SRC will be used as
the filename of the downloaded resource.

The function returns the absolute path to the file when it is newly
created. It returns nil, if download was skipped as the file already
exists."
  (let ((dest (expand-file-name (or dest ".")))
        dirpath filename)
    (if (file-directory-p dest) ; is dest an existing directory?
        (setq dirpath dest
              filename (file-name-nondirectory src))
      (if (file-directory-p (file-name-directory dest))
          (setq dirpath (file-name-directory dest)
                filename (file-name-nondirectory dest))
        (error "%s is not a directory" (file-name-directory dest))))
    (setq dest (file-name-concat dirpath filename))
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

;;; Directory Local Variables

(defun ok-file-hack-dir-local-variables--ad (fun)
  "Walk up the directory tree to load all directory local variables.

The standard function `hack-dir-local-variables' stops looking for
`dir-locals-file' after the first one found in the current directory
tree, ignoring ones in parent directories. This function adds an
enhancement such that all `dir-locals-file' files are loaded in their
order in the directory tree, i.e., from top to bottom, enabling all to
be loaded without redefinition in subdirectories.

Note that this behavior is slightly different from that of hook
functions in `hack-dir-local-get-variables-functions'. See the
`hack-dir-local-variables' code for the difference in variable loading
behavior.

Use this function as an around advice for `hack-dir-local-variables',
which FUN refers to."
  (let* ((orig-buffer-file-name (buffer-file-name))
         (dpaths (ok-file-locate-dominating-files (or orig-buffer-file-name
                                                      default-directory)
                                                  dir-locals-file)))
    (unwind-protect
        (dolist (dpath dpaths)
          ;; The following fakes visiting a file up the hierarchy:
          (setq buffer-file-name (file-name-concat dpath dir-locals-file))
          (funcall fun))
      (setq buffer-file-name orig-buffer-file-name))))

(defun ok-file-hack-dir-local--get-variables (&optional predicate)
  "Get all per-directory variables by walking up the directory tree.
The variable getter function extends `hack-dir-local--get-variables' by not
stopping at the first `dir-locals-file' found. It walks up the directory tree to
find all of them, merging variables based on their proximity of definitions to
the location of the current file, i.e., closely defined variables take
precedence. For the merging logic, see `hack-dir-local-variables' code.

Add this hook function to `hack-dir-local-get-variables-functions', in place or
in addition to `hack-dir-local--get-variables'. See the documentation of
`hack-dir-local-get-variables-functions' for detail on its return value.

PREDICATE is passed to `dir-locals-collect-variables'."
  (mapcar
   (lambda (dir)
     (let ((buffer-file-name (file-name-concat dir dir-locals-file)))
       (hack-dir-local--get-variables predicate)))
   (let ((parent-dir (or (buffer-file-name) default-directory))
         dirs)
     (while-let ((dir (when parent-dir
                        (locate-dominating-file parent-dir dir-locals-file))))
       (push dir dirs)
       (setq parent-dir (unless (or (string= "/" dir) (string-empty-p dir))
                          (file-name-directory
                           (directory-file-name (expand-file-name dir))))))
     (mapcar #'expand-file-name dirs))))

;; Reloading utilities (see https://emacs.stackexchange.com/a/13096/599)

(defvar default-directory-symlinked nil
  "Set `default-directory' for symlinked `dir-locals-file'.
Set this within symlinked `dir-locals-file'.")

(defun ok-file-dir-locals-reload-for-current-buffer ()
  "Reload currently visited .dir-locals.el buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables)))

(defun ok-file-dir-locals-reload-for-all-buffers ()
  "Reload `dir-locals-file' for all buffers in `default-directory'."
  (interactive)
  (let ((dir (or default-directory-symlinked default-directory)))
    (dolist (buffer (buffer-list))
      (when (file-in-directory-p (buffer-file-name buffer) dir)
        (with-current-buffer buffer
          (hack-dir-local-variables))))))

;;; Utilities for `safe-local-variable-directories'

(defun ok-file-safe-local-variable-directories-add ()
  "Add the current directory to `safe-local-variable-directories'."
  (interactive)
  (when default-directory
    (add-to-list 'safe-local-variable-directories default-directory)))

(defun ok-file-safe-local-variable-directories-remove (&optional d)
  "Remove a directory D from `safe-local-variable-directories'.
If D is not given, the function will prompt for it interactively."
  (interactive)
  (let ((d (or d
               (completing-read "Select directory to remove: "
                                safe-local-variable-directories))))
    (setq safe-local-variable-directories
          (delete d safe-local-variable-directories))))

;;; Extend `rename-visited-file'

(defun ok-file--rename-visited-ad (fun &optional new-location)
  "Around advise FUN to provide NEW-LOCATION contextually.
In an `org-mode' document, the new name will be suggested from the document
title. Otherwise, it delegates to `rename-visited-file' normally."
  (interactive
   (list
    (if-let* ((_ (derived-mode-p '(org-mode)))
              (title (cadr (assoc "TITLE" (org-collect-keywords '("title")))))
              (filename (format "%s.org" (ok-string-text-to-slug title))))
        (read-file-name "Rename visited Org file name: "
                        default-directory
                        (expand-file-name filename default-directory)
                        nil
                        filename))))
  (if new-location
      (apply fun (list new-location))
    (call-interactively fun)))

(advice-add #'rename-visited-file :around #'ok-file--rename-visited-ad)

(provide 'ok-file)
;;; ok-file.el ends here
