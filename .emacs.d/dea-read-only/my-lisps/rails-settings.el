;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/31/2009 00:33:35 ÐÇÆÚÁù by ahei>

(require 'rails)
(require 'two-mode-mode)
(require 'rhtml-minor-mode)
(require 'rhtml-mode)

(define-key rhtml-mode-map (kbd "TAB") 'complete-or-indent-for-ruby)
(defun rhtml-modes ()
  (two-mode-mode)
  (rhtml-minor-mode))
(setq auto-mode-alist (cons '("\\.rhtml$" . rhtml-modes) auto-mode-alist))

(define-key compilation-mode-map "u" 'View-scroll-page-backward)

(setq auto-mode-alist (cons '("\\.jsp$" . java-mode) auto-mode-alist))

(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))
