;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/22/2009 10:37:08 星期四 by ahei>

(defvar xref-current-project nil)
(defvar xref-key-binding 'global)
(setq exec-path (cons (concat my-emacs-lisps-path "xref") exec-path))
(load "xrefactory")

;; 查找定义
(global-set-key (kbd "C-c x d") 'xref-push-and-goto-definition)
;; 返回
(global-set-key (kbd "C-c x r") 'xref-pop-and-return)
;; 浏览符号
(global-set-key (kbd "C-c x b") 'xref-browse-symbol)
;; 上一个引用
(global-set-key (kbd "C-c x ,") 'xref-previous-reference)
;; 下一个引用
(global-set-key (kbd "C-c x .") 'xref-next-reference)
