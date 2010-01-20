;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/21/2009 10:30:20 星期三 by ahei>

(require 'recentf)

(setq recentf-max-saved-items nil)
(recentf-mode 1)
(defvar recentf-open-last-file "" "`recentf-open-files-complete'最近打开的文件")
(defun recentf-open-files-complete ()
  (interactive)
  (let* ((all-files recentf-list)
         (default (file-name-nondirectory (directory-file-name recentf-open-last-file)))
         (collection (mapcar (function (lambda (x) (cons (file-name-nondirectory (directory-file-name x)) x))) all-files))
         (prompt (if (string= default "") "文件名或目录名: " (format "文件名或目录名(缺省为%s): " default)))
         (file ""))
    (while (string= file "")
         (setq file (completing-read prompt collection nil t nil nil default)))
    (find-file (setq recentf-open-last-file (cdr (assoc-ignore-representation file collection))))))
(global-set-key [(control x)(control r)] 'recentf-open-files-complete-sb)
(define-key recentf-dialog-mode-map (kbd "n") 'widget-forward)
(define-key recentf-dialog-mode-map (kbd "j") 'widget-forward)
(define-key recentf-dialog-mode-map (kbd "p") 'widget-backward)
(define-key recentf-dialog-mode-map (kbd "k") 'widget-backward)

;; 记录打开的目录到recentf里面去
(defun recentf-add-dir ()
  "Add directory name to recentf file list."
  (recentf-add-file dired-directory))

(add-hook 'dired-mode-hook 'recentf-add-dir)
