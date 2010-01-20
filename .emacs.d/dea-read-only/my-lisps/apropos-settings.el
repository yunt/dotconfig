;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-09 17:55:14 Monday by ahei>

(defvar apropos-mode-map-key-pairs
  `(("u" scroll-down)
    ("n" forward-button)
    ("p" backward-button))
  "*Key pairs for `apropos-mode-map'.")

(apply-map-define-keys 'apropos-mode-map)
