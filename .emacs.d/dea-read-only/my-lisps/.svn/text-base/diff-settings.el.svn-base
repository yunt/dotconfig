;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-01-09 14:04:08 Saturday by ahei>

(require 'diff-mode)

(apply-define-key
 diff-mode-map
 `(("C-k" diff-hunk-kill)
   ("SPC" scroll-up)
   ("'"   switch-to-other-buffer)
   ("Q"   kill-this-buffer)
   ("u"   View-scroll-half-page-backward)))

(apply-define-key
 diff-mode-shared-map
 `(("k" previous-line)
   ("K" roll-up)))

(setq diff-switches "-w")
