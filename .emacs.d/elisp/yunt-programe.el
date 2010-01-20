;;; yunt-programe.el ---

;; 增加自定义关键字
(dolist (mode '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode lisp-interaction-mode sh-mode
                       sgml-mode))
  (font-lock-add-keywords mode
                          '(("\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\):" 1 font-lock-warning-face prepend)
                            ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
                            ("(\\|)" . beautiful-blue-face)
                            ("\\[\\|]" . yellow-face)
                            ("<\\|>" . cyan-face)
                            ("{\\|}" . green-face))))
(font-lock-add-keywords 'ruby-mode '(("\\<require\\>" . font-lock-keyword-face)))

(require 'yasnippet)

(setq yas/root-directory (concat my-emacs-path "snippets"))
(yas/load-directory yas/root-directory)
(setq yas/trigger-key nil)
(yas/global-mode t)

(define-key yas/keymap (kbd "M-j") 'yas/next-field-or-maybe-expand)
(define-key yas/keymap (kbd "M-k") 'yas/prev-field)

;;; sh
(require 'sh-script)

(defvar sh-mode-map-key-pairs
  `(("<" self-insert-command)
    ("C-c M-c" sh-case)
    ("C-c C-c" comment)
    ("C-c g" bashdb))
  "*Key pairs for `sh-mode'.")

(apply-map-define-keys 'sh-mode-map)

(font-lock-add-keywords 'sh-mode '(("\\<\\(local\\|let\\)\\>" . font-lock-keyword-face)))

;; 简写模式
(setq-default abbrev-mode t)
(setq save-abbrevs nil)
;; 超强的abbrev
(load "msf-abbrev-settings")

;;; C & C++
;;; 当输入"."或">"时，在另一个窗口中列出结构体或类的成员
(defun my-c-mode-cedet-hook ()
  (setq tab-width 4 indent-tabs-mode nil)
  (c-toggle-auto-hungry-state 1)
;;  (c-set-style "gnu")
  (c-set-offset 'member-init-intro '++)
  (font-lock-mode 2)
  (define-key c-mode-base-map "C-m" 'newline-and-indent)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu))
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;;; yunt-programe.el ends here
