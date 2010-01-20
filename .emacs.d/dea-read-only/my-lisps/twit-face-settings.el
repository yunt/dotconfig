;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-21 10:09:50 Monday by ahei>

(custom-set-faces
 '(twit-message-face
   ((((type tty pc)) :foreground "yellow")
    (t :foreground "#FFFFFEA78873"))))

(custom-set-faces
 '(twit-author-face
   ((((type tty pc)) :foreground "magenta")
    (t :foreground "#F1C280F1FFFF"))))

(custom-set-faces
 '(twit-info-face
   ((((type tty pc)) :height 0.8 :bold t :foreground "blue")
    (t :height 0.8 :foreground "#D58EE0FAFFFF"))))

(custom-set-faces
 '(twit-title-face
   ((((background light))
     :background "PowderBlue" :bold t
     :box (:line-width 2 :color "PowderBlue" :style 0))
    (((background dark))
     :background "#D58EFFFFFC18" :foreground "blue")
    (t :underline "white"))))

(custom-set-faces
 '(twit-zebra-1-face
   ((((class color) (background light))
     :foreground "black" :background "gray89"
                  :box (:line-width 2 :color "gray89" :style 0))
    (((class color) (background dark))
     :foreground "red" :background "black"
                  :box (:line-width 2 :color "black" :style 0))
    (t :inverse))))

(custom-set-faces
 '(twit-zebra-2-face
   ((((class color) (background light))
     (:foreground "black" :background "AliceBlue"
                  :box (:line-width 2 :color "AliceBlue" :style 0)))
    (((class color) (background dark))
     (:foreground "green" :background "grey4"
                  :box (:line-width 2 :color "grey4" :style 0))))))

(custom-set-faces '(twit-url-face ((t))))
