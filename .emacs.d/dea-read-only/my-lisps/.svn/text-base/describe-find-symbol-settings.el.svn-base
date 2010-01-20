;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-09 23:32:15 Monday by ahei>

(require 'debug)

(defun apropos-displn (pattern &optional do-all)
  "先执行`apropos', 然后执行`displn-mode'"
  (interactive (list (apropos-read-pattern "symbol") current-prefix-arg))
  (apropos pattern do-all)
  (with-current-buffer (get-buffer "*Apropos*") (displn-mode t)))
(defun apropos-command-displn (pattern &optional do-all var-predicate)
  "先执行`apropos-command', 然后执行`displn-mode'"
  (interactive
   (list
    (apropos-read-pattern (if (or current-prefix-arg apropos-do-all) "命令或者函数" "命令"))
    current-prefix-arg))
  (apropos-command pattern do-all var-predicate)
  (with-current-buffer (get-buffer "*Apropos*") (displn-mode t)))
(defun apropos-command-fun-displn (pattern &optional var-predicate)
  "先执行`apropos-command' do-all, 然后执行`displn-mode'"
  (interactive (list (apropos-read-pattern "命令或者函数")))
  (apropos-command pattern t var-predicate)
  (with-current-buffer (get-buffer "*Apropos*") (displn-mode t)))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map completion-list-mode-map help-mode-map
                   debugger-mode-map))
  (let ((key-pairs
         `(("C-c /"   describe-symbol-at-point)
           ("C-c M-v" describe-variable-at-point)
           ("C-c M-f" describe-function-at-point)
           ("C-c M-s" describe-face-at-point)
           ("C-c ."   find-symbol-at-point)
           ("C-c ,"   find-symbol-go-back)
           ("C-c M-V" find-symbol-var-at-point)
           ("C-c M-F" find-symbol-fun-at-point)
           ("C-c M-S" find-symbol-face-at-point)
           ("C-c w"   where-is-at-point))))
    (apply-define-key map key-pairs)))

(let ((key-pairs
       `(("C-x C-k" describe-key-sb)
         ("C-x C-m" describe-mode)
         ("C-x / A" describe-face)
         ("C-x / a" apropos-displn)
         ("C-x A"   apropos-command-fun-displn)
         ("C-x C-d" find-symbol-sb)
         ("C-x K"   find-symbol-fun-on-key-sb)
         (,(if window-system "C-x C-/" "C-x C-_") describe-symbol-sb))))
  (apply-define-key global-map key-pairs))
