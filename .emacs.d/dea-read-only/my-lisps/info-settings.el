;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-01-09 14:22:34 Saturday by ahei>

(require 'info)

(defvar Info-mode-map-key-pairs
  `(("j"     next-line)
    ("k"     previous-line)
    ("h"     backward-char)
    ("l"     forward-char)
    ("U"     Info-up)
    ("u"     View-scroll-half-page-backward)
    ("Q"     kill-this-buffer)
    ("o"     other-window)
    ("SPC"   scroll-up)
    ("C-h"   Info-up)
    ("N"     Info-next-reference)
    ("P"     Info-prev-reference)
    ("'"     switch-to-other-buffer)
    ("C-c ," Info-history-back)
    ("C-c ." Info-history-forward))
  "*Keys map for `Info-mode'.")

(apply-map-define-keys 'Info-mode-map)

(apply-args-list-to-fun
 'def-command-max-window
 `("info"))

(apply-define-key
 global-map
 `(("C-x I" info-max-window)))
