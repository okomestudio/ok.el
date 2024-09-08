;;; ok-file.el --- Okome Studio file utilities  -*- lexical-binding: t -*-
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

(defun ok-file-locate-dominating-files (file name)
  "Look upward from FIL in directory hierarchy to locate files named NAME.
This extends the `locate-dominating-file` function not to stop at
the first occurrence of NAME and continues looking upward in the
directory tree."
  (let* ((dir-locals-file (locate-dominating-file file name))
         (dir-locals-files '())
         (parent-dir nil))
    (while dir-locals-file
      (push dir-locals-file dir-locals-files)
      (setq parent-dir (ok-file-parent-directory dir-locals-file))
      (setq dir-locals-file
            (cond ((not (eq parent-dir nil)) (locate-dominating-file parent-dir name))
                  (t nil))))
    dir-locals-files))

(defun ok-file-parent-directory (path)
  "Get the parent directory of PATH.
This function effectively removes the last component from PATH,
regardless of the path pointing to a file or a directory. The
returned directory always ends with a '/' character. The function
returns nil if no parent directory exists (i.e., PATH points to
root)."
  (unless (equal "/" path)
    (file-name-directory (directory-file-name (expand-file-name path)))))

(provide 'ok-file)
;;; ok-file.el ends here
