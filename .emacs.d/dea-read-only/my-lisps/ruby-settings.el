;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/31/2009 01:06:36 ÐÇÆÚÁù by ahei>

(require 'ruby-mode)

(define-key ruby-mode-map (kbd "TAB") 'complete-or-indent-for-ruby)
(defun complete-or-indent-for-ruby (arg)
  (interactive "P")
  (complete-or-indent arg nil 'ruby-indent-command))
(define-key ruby-mode-map (kbd "C-j") 'goto-line)
(define-key ruby-mode-map (kbd "C-c C-c") 'comment)
(define-key ruby-mode-map (kbd "{") 'self-insert-command)
(define-key ruby-mode-map (kbd "}") 'self-insert-command)
(define-key global-map (kbd "C-x r") 'run-ruby)
(defun ruby-keys ()
  "Ruby keys definition."
  (local-set-key (kbd "<return>") 'newline-and-indent))
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-indent-level 4)
            (ruby-electric-mode nil)
            (ruby-keys)) t)
(defalias 'irb 'run-ruby)
(defun ruby-mark-defun ()
  "Put mark at end of this Ruby function, point at beginning."
  (interactive)
  (push-mark (point))
  (ruby-end-of-defun)
  (push-mark (point) nil t)
  (ruby-beginning-of-defun))

(setq ri-ruby-script (concat my-emacs-lisps-path "ri-emacs.rb"))
(setq ri-ruby-script "/home/ahei/emacs/lisps/ri-emacs.rb")
(autoload 'ri "ri-ruby.el" nil t)
(define-key global-map (kbd "C-x R") 'ri)
