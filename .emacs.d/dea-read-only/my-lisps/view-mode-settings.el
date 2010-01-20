;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-02 01:45:57 Wednesday by ahei>

;; 为`view-mode'加入vi的按键
(global-set-key (kbd "M-S") 'exit-view)
(defun view ()
  "进去view-mode"
  (interactive)
  (view-mode t)
  (setq buffer-read-only nil))
(defun exit-view ()
  "退出`view-mode'"
  (interactive)
  (view-mode -1))
(defun enable-view-edit ()
  (setq buffer-read-only nil))
(add-hook 'view-mode-hook 'enable-view-edit)

(global-set-key (kbd "M-s") 'view)
(define-key view-mode-map "U" 'View-scroll-page-backward)
(define-key view-mode-map "/" 'describe-symbol-at-point)
(define-key view-mode-map "\C-c\C-c" 'comment-in-view)
(define-key view-mode-map "\ey" 'yank-pop-rdonly)
(define-key view-mode-map (kbd "RET") 'newline-and-indent)
(define-key view-mode-map "x" 'delete-char)
(add-hook 'view-mode-hook (lambda () (define-key view-mode-map "\t" 'indent-for-tab-command-rdonly)))
(define-key view-mode-map "\C-j" 'goto-line)
(define-key view-mode-map (kbd "I") 'bol-and-quit-view)
(define-key view-mode-map (kbd "A") 'eol-and-quit-view)
(define-key view-mode-map (kbd "O") 'newline-and-quit-view)
(define-key view-mode-map (kbd "'") 'switch-to-other-buffer)
(define-key view-mode-map (kbd "m") 'back-to-indentation)
(define-key view-mode-map (kbd "p") 'previous-line-or-backward-button)
(define-key view-mode-map (kbd "Q") 'kill-this-buffer)
(define-key view-mode-map (kbd "L") 'count-brf-lines)
(define-key view-mode-map (kbd "1") 'delete-other-windows)
(define-key view-mode-map (kbd "t") 'sb-toggle-keep-buffer)
(define-key view-mode-map (kbd "N") 'select-buffer-forward)
(define-key view-mode-map (kbd "P") 'select-buffer-backward)
(define-key view-mode-map (kbd "<backspace>") 'c-electric-backspace)
(define-key view-mode-map (kbd ",") 'find-symbol-go-back)
(defun previous-line-or-backward-button ()
  "`major-mode'为`help-mode'时, 执行`backward-button', 否则执行`previous-line'."
  (interactive)
  (if (equal major-mode 'help-mode)
      (call-interactively 'backward-button)
    (call-interactively 'previous-line)))

(setq edebug-active nil)

(defun F-command ()
  (interactive)
  (if (eq major-mode 'help-mode)
      (call-interactively 'help-go-forward)
    (call-interactively 'View-revert-buffer-scroll-page-forward)))

(defun q-command ()
     (interactive)
     (if edebug-active
         (top-level)
       (if (equal major-mode 'help-mode)
           (bury-buffer)
         (View-quit))))

(define-key view-mode-map (kbd "F") 'F-command)
(define-key view-mode-map (kbd "q") 'q-command)
(define-key view-mode-map (kbd "i") 'q-command)
(define-key view-mode-map (kbd "B") 'eval-buffer)
(define-key view-mode-map (kbd "n")
  '(lambda (n)
     (interactive "p")
     (if edebug-active
         (edebug-next-mode)
     (if (equal major-mode 'gud-mode)
         (gud-next n)
       (if (equal major-mode 'help-mode)
           (call-interactively 'forward-button)
         (call-interactively 'next-line))))))
(define-key view-mode-map (kbd "s")
  '(lambda (&optional regexp-p no-recursive-edit)
     (interactive "P\np")
     (if (equal major-mode 'gud-mode)
         (gud-step (prefix-numeric-value current-prefix-arg))
       (isearch-forward regexp-p no-recursive-edit))))
(define-key view-mode-map (kbd "r")
  '(lambda (&optional regexp-p no-recursive-edit)
     (interactive "P\np")
     (if (equal major-mode 'gud-mode)
         (gud-run (prefix-numeric-value current-prefix-arg))
       (isearch-backward regexp-p no-recursive-edit))))
(define-key view-mode-map (kbd "f")
  '(lambda (arg)
     (interactive "p")
     (if (equal major-mode 'gud-mode) (gud-finish arg) (forward-word arg))))
(define-key view-mode-map (kbd "b")
  '(lambda (arg)
     (interactive "p")
     (if (equal major-mode 'gud-mode) (gud-break arg) (backward-word arg))))
(define-key view-mode-map (kbd "d")
  '(lambda (arg)
     (interactive "P")
     (if (equal major-mode 'gud-mode) (gud-remove (prefix-numeric-value arg)) (View-scroll-half-page-forward arg))))
(defun l-command ()
     (interactive)
     (if (equal major-mode 'gud-mode) (call-interactively 'gud-refresh) (call-interactively 'forward-char)))
(define-key view-mode-map (kbd "l") 'l-command)
(defun c-command (beg end)
  (interactive "r")
  (if edebug-active
      (edebug-continue-mode)
    (if (equal major-mode 'gud-mode)
        (gud-cont (prefix-numeric-value current-prefix-arg))
      (copy-region-as-kill beg end))))

(define-key view-mode-map (kbd "c") 'c-command)
(define-key view-mode-map (kbd "y")
  '(lambda (n)
     (interactive "p")
     (if (equal major-mode 'gud-mode)
         (progn
           (setq buffer-read-only nil)
           (insert "y")
           (comint-send-input)
           (setq buffer-read-only t))
       (copy-region-as-kill (region-beginning) (region-beginning)))))
(define-key view-mode-map (kbd "SPC")
  '(lambda (lines)
     (interactive "P")
     (if (equal major-mode 'gud-mode)
         (progn
           (setq buffer-read-only nil)
           (insert "y")
           (comint-send-input)
           (setq buffer-read-only t))
       (View-scroll-page-forward lines))))

(define-key view-mode-map (kbd ".")
  '(lambda ()
     (interactive)
     (if (or
          (equal major-mode 'emacs-lisp-mode)
          (equal major-mode 'lisp-mode)
          (equal major-mode 'lisp-interaction-mode)
          (equal major-mode 'help-mode)
          (equal major-mode 'completion-list-mode))
         (call-interactively 'find-symbol-at-point)
       (call-interactively 'set-mark-command))))

(defun bol-and-quit-view ()
  "退出`view-mode'然后执行`back-to-indentation', 相当于vi里面的命令模式下的I命令"
  (interactive)
  (View-quit)
  (back-to-indentation))
(defun eol-and-quit-view ()
  "退出`view-mode'然后执行`end-of-line', 相当于vi里面的命令模式下的A命令"
  (interactive)
  (View-quit)
  (end-of-line))
(defun newline-and-quit-view ()
  "退出`view-mode'然后执行`end-of-line', 再执行`newline-and-indent', 相当于vi里面的命令模式下的o命令"
  (interactive)
  (View-quit)
  (end-of-line)
  (newline-and-indent))

(defface view-mode-mode-line-face
  '((((type tty pc)) :bold t :foreground "red" :background "white") (t (:background "red" :foreground "white")))
  "Face used highlight `view-mode-mode-line-format'.")

(defvar view-mode-mode-line-format
  (propertize "View"
              'local-map mode-line-minor-mode-keymap
              'help-echo "mouse-3: minor mode menu"
              'face 'view-mode-mode-line-face)
  "*Mode line format of `view-mode'.")

;; 这个是关键, 没有这个显示不出来颜色
(put 'view-mode-mode-line-format 'risky-local-variable t)

(setq minor-mode-alist
      (append
       `((view-mode " ") (view-mode ,view-mode-mode-line-format))
       (delq (assq 'view-mode minor-mode-alist) minor-mode-alist)))
