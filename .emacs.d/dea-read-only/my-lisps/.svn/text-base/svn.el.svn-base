;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-09 16:45:22 星期一 by ahei>

(defun svn-dired-jump ()
  "Jump to a dired buffer, containing the file at point."
  (interactive)
  (let* ((line-info (svn-status-get-line-information))
         (file-full-path
          (if line-info
              (setq file-full-path (svn-status-line-info->full-path line-info))))
         (default-directory
           (if line-info
               (file-name-as-directory
                (expand-file-name (svn-status-line-info->directory-containing-line-info line-info t)))
             default-directory)))
    (dired-jump)
    (if line-info
        (dired-goto-file file-full-path))))

(defun svn-delete-files ()
  "Delete selected file(s).
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (let* ((marked-files (svn-status-marked-files))
         (fnames (mapcar 'svn-status-line-info->full-path marked-files))
         (num-of-files (length marked-files)))
    (when (yes-or-no-p
           (if (= 1 num-of-files)
               (format "Remove %s? " (svn-status-line-info->filename (car marked-files)))
             (format "Remove %d files? " num-of-files)))
      (message "removing: %S" (svn-status-marked-file-names))
      (mapc 'delete-file fnames)
      (call-interactively 'svn-status-update))))

(defun svn-cleanup (files)
  "Run `svn cleanup' on files FILES."
  (interactive "P")
  (if files
      (progn
        (message "svn-status-cleanup %S" files)
        (svn-run-svn t t 'cleanup (append (list "cleanup") files)))
    (message "No valid file selected - No status cleanup possible")))

(defun svn-cleanup-current-dir ()
  "Run `svn cleanup' on curent directory."
  (interactive)
  (svn-cleanup `(,default-directory)))

(defun svn-create-arg-file (file-name prefix files postfix)
  (with-temp-file file-name
    (insert prefix)
    (let ((temp-files files))
      (while temp-files
        (insert (car temp-files))
        (insert "\n")
        (setq temp-files (cdr temp-files)))
      (insert postfix))))

(provide 'svn)
