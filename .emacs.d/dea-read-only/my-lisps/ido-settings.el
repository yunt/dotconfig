;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-01-07 17:04:03 Thursday by ahei>

(if is-before-emacs-21
    (progn
      (load "ido-for-21")
      (setq read-buffer-function 'ido-read-buffer))
  (require 'ido)
  (ido-everywhere t)
  (setq ido-define-mode-map-hook 'ido-setup-hook))

(add-hook ido-define-mode-map-hook 'ido-keys)

(ido-mode t)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(setq ido-max-directory-size 1000000)

(defmacro def-ido-enter-command (command)
  "Make definition of command which execute some command in ido."
  `(defun ,(concat-name "ido-enter-" command) ()
     ,(concat "Drop into `" command "' from file switching.")
     (interactive)
     (setq ido-exit (quote ,(intern command)))
     (exit-minibuffer)))

(defvar commands-execute-in-ido `("svn-status-hide") "*Commands execute in ido.")

(apply-args-list-to-fun 'def-ido-enter-command commands-execute-in-ido)

(defun ido-up-directory-clean-text ()
  "Run C-u `ido-up-directory'."
  (interactive)
  (ido-up-directory t))

(defun ido-keys ()
  "`ido'的按键设置"
  (let ((map
         (unless is-before-emacs-21
           (setq ido-mode-map ido-completion-map))))
        (apply-define-key
         map 
         `(("M-." ido-next-match-dir)
           ("M-," ido-prev-match-dir)
           ("C-h" ido-delete-backward-updir)
           ("M-h" ido-up-directory)
           ("M-H" ido-up-directory-clean-text)
           ("C-w" ido-delete-backward-word-updir)
           ("C-v" ido-enter-svn-status-hide)
           ("C-n" ido-next-match)
           ("C-p" ido-prev-match)))))
