;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-04 14:49:49 Friday by ahei>

(require 'grep)

(defun find-grep-in-dir (dir)
  "Run `find-grep' in directory DIR."
  (interactive (list (read-directory-name "Directory to find in: " default-directory "" t)))
  (let ((prompt (concat "find " dir " -type f ! -wholename \"*/.svn*\" ! -wholename \"*~\" -print0 | xargs -0 -e grep -nH -e ")))
    (if is-after-emacs-23
        (grep-apply-setting 'grep-find-command prompt)
      (setq grep-find-command prompt))
    (call-interactively 'find-grep)))

(unless is-before-emacs-21
  (define-key global-map (kbd "C-x F") 'find-grep)
  (define-key global-map (kbd "C-x f") 'find-grep-in-dir)
  (define-key grep-mode-map (kbd "q") 'bury-buffer)
  (define-key grep-mode-map (kbd "Q") 'kill-this-buffer)
  (define-key grep-mode-map (kbd "u") 'scroll-down)
  (define-key grep-mode-map (kbd "/") 'describe-symbol-at-point)
  (define-key grep-mode-map (kbd "t") 'sb-toggle-keep-buffer)
  (define-key grep-mode-map (kbd "N") 'select-buffer-forward)
  (define-key grep-mode-map (kbd "P") 'select-buffer-backward)
  (define-key grep-mode-map (kbd "L") 'count-brf-lines))

(defvar grep-find-prompt "find . -type f ! -wholename \"*/.svn*\" ! -wholename \"*~\" -print0 | xargs -0 -e grep -nH -e "
  "*Default prompt of `grep-find'.")

(if is-after-emacs-23
    (grep-apply-setting 'grep-find-command grep-find-prompt)
  (setq grep-find-command grep-find-prompt))

(defvar grep-ignore-case nil "When run `grep' ignore case or not.")

(when grep-ignore-case
  (if is-after-emacs-23
      (grep-apply-setting 'grep-command "grep -inH -e ")
    (setq grep-command "grep -inH -e ")))

(defun grep-current-word (dir &optional is-prompt)
  "Run `grep' to find current word in directory DIR."
  (interactive
   (list
    (read-directory-name "Directory to grep in: " default-directory "" t)
    current-prefix-arg))
  (let* ((word (grep-tag-default))
         (commands (concat grep-command word " " dir "/*")))
    (if is-prompt
        (grep (read-shell-command "Run grep (like this): " commands 'grep-history))
        (grep commands))))

(defun grep-current-word-in-current-dir (&optional is-prompt)
  "Run `grep' to find current word in directory DIR."
  (interactive "P")
  (grep-current-word default-directory is-prompt))
