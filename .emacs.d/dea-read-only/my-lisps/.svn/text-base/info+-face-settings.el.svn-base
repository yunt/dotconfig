;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-08 14:58:00 星期日 by ahei>

(require 'info+)

(if use-black-background
    (progn
      (set-face-foreground 'info-string "magenta"))
  (set-face-foreground 'info-string "blue"))
(custom-set-faces '(info-quoted-name
                    ((((type tty)) :bold t :foreground "green")
                     (t :foreground "cornflower blue"))))
(set-face-background 'info-single-quote "red")
(set-face-foreground 'info-single-quote "white")
(custom-set-faces '(info-reference-item
                    ((((type tty pc)) :background "white" :foreground "black")
                     (t :background "white" :foreground "cornflower blue"))))
(set-face-foreground 'info-function-ref-item "deeppink1")

(define-key Info-mode-map [mouse-4] 'mwheel-scroll)
(define-key Info-mode-map [mouse-5] 'mwheel-scroll)
