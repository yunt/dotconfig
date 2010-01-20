;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-01-09 17:30:13 Saturday by ahei>

(require 'mark-settings)

(setq use-cua t)

(setq cua-rectangle-mark-key "")

(when is-after-emacs-23
  (setq cua-remap-control-z nil)
  (setq cua-remap-control-v nil))

(apply-args-list-to-fun
 'def-mark-move-command
 `("cua-resize-rectangle-down"
   "cua-resize-rectangle-up"
   "cua-resize-rectangle-right"
   "cua-resize-rectangle-left"
   "cua-resize-rectangle-top"
   "cua-resize-rectangle-page-up"
   "cua-resize-rectangle-page-down"
   "cua-resize-rectangle-bot"))

(autoload 'cua--init-rectangles "cua-rect")
(eval-after-load "cua-base"
  '(progn
     (unless is-after-emacs-23
       (define-key cua--cua-keys-keymap [(control z)] nil)
       (define-key cua--cua-keys-keymap [(control v)] nil)
       (define-key cua--cua-keys-keymap [(meta v)] nil))
     
     (cua--init-rectangles)
     (apply-define-key
      cua--rectangle-keymap
      `(("M-f"     forward-word-remember)
        ("M-b"     backward-word-remember)
        ("C-c C-f" cua-fill-char-rectangle)
        ("'"       cua-insert-char-rectangle)))))

