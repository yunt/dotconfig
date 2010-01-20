;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-03 13:27:52 Thursday by ahei>

;; 在只读buffer中执行编辑命令

(defvar read-only nil "编辑命令运行前buffer是否为`buffer-read-only'")

(defvar commands-with-rdonly
  '(kill-line
    kill-region
    undo
    backward-kill-word-or-kill-region
    kill-sexp
    newline-and-indent
    yank
    yank-pop
    save-buffer
    comment
    delete-char
    c-electric-backspace
    kill-word
    delete-indentation
    indent-region
    open-line
    insert-cur-line
    insert-cur-sexp
    kill-whole-line
    kill-sexp)
  "*运行命令前需要把`buffer-read-only'设为nil的命令")

(defun rdonly-pre-command ()
  (when (memq this-command commands-with-rdonly)
    (setq read-only buffer-read-only)
    (setq buffer-read-only nil)))

(defun rdonly-post-command ()
  (when (memq this-command commands-with-rdonly)
    (setq buffer-read-only read-only)))

(add-hook 'pre-command-hook 'rdonly-pre-command)
(add-hook 'post-command-hook 'rdonly-post-command)

(defun comment-in-view (&optional arg)
  "在只读buffer中执行`comment'"
  (interactive "P")
  (if (equal major-mode 'help-mode)
      (help-follow-symbol (point))
    (setq buffer-read-only nil)
    (comment arg)
    (setq buffer-read-only t)))

(defun yank-pop-rdonly (&optional arg)
  "在只读buffer中执行`yank-pop-with-displn'"
  (interactive "p")
  (setq buffer-read-only nil)
  (let ((buffer (current-buffer)))
    (yank-pop arg)
    (switch-to-buffer buffer)
    (setq buffer-read-only t)
    (if (not (eq last-command 'yank))
        (switch-to-buffer "*Kill Ring*"))))

(defun browse-kill-ring-insert-and-quit-rdonly ()
  "在只读buffer中执行`browse-kill-ring-insert-and-quit'"
  (interactive)
  (let ((original-buffer (window-buffer browse-kill-ring-original-window)) is-rdonly)
    (with-current-buffer original-buffer (setq is-rdonly buffer-read-only))
    (if (not is-rdonly)
        (browse-kill-ring-insert-and-quit)
      (with-current-buffer original-buffer (setq buffer-read-only nil))
      (browse-kill-ring-insert-and-quit)
      (setq buffer-read-only t))))

(defun indent-for-tab-command-rdonly (n &optional wrap display-message)
  "在只读buffer中执行`indent-for-tab-command'"
  (interactive "p\nd\nd")
  (setq ro buffer-read-only)
  (setq buffer-read-only nil)
  (cond
   ((member major-mode '(lisp-mode emacs-lisp-mode c-mode c++-mode sh-mode
                                   java-mode jde-mode sgml-mode html-mode))
    (indent-for-tab-command))
   ((equal major-mode 'help-mode)
    (if is-before-emacs-21 (help-next-ref) (forward-button n wrap display-message))))
  (setq buffer-read-only ro))
