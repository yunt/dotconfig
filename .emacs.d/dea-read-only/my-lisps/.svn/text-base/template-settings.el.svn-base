;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-13 22:24:19 Friday by ahei>

(require 'template)

(template-initialize)
(setq template-default-directories (list (concat my-emacs-path "/templates/")))
(defvar last-template nil "最近使用的模版文件")
(defun my-template-expand-template (template)
  "展开template的模版文件"
  (interactive
   (list
    (read-file-name
     (if last-template (format "请指定模版文件(缺省为%s): " last-template) "请指定模版文件: ")
     (concat my-emacs-path "templates") last-template t)))
  (template-expand-template template)
  (setq last-template template))
(dolist (map (list emacs-lisp-mode-map c-mode-base-map makefile-mode-map makefile-automake-mode-map
                   sh-mode-map text-mode-map))
  (define-key map (kbd "C-c T") 'my-template-expand-template)
  (define-key map (kbd "C-c C-t") 'template-expand-template))
