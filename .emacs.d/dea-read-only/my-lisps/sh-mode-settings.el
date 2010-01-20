;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-10 14:12:08 Tuesday by ahei>

(require 'sh-script)

(defvar sh-mode-map-key-pairs
  `(("<" self-insert-command)
    ("C-c M-c" sh-case)
    ("C-c C-c" comment)
    ("C-c g" bashdb))
  "*Key pairs for `sh-mode'.")

(apply-map-define-keys 'sh-mode-map)

(font-lock-add-keywords 'sh-mode '(("\\<\\(local\\|let\\)\\>" . font-lock-keyword-face)))
