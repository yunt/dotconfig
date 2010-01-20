;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-01-09 22:45:50 Saturday by ahei>

(require 'util)
(require 'eagle)

(setq eagle-twitter-username "ahei0802")
(setq twit-show-user-images t)
(add-hook 'eagle-new-tweet-hook 'eagle-todochiku)

(defun eagle-settings ()
  (make-local-variable 'hl-line-face)
  (setq hl-line-face 'hl-line-nonunderline-face)
  (setq hl-line-overlay nil)
  (color-theme-adjust-hl-line-face))

(add-hook 'eagle-mode-hook 'eagle-settings)

(apply-define-key
 eagle-mode-map
 `(("'"       switch-to-other-buffer)))

(apply-define-key
 global-map
 `(("C-x M-t" eagle-show-recent-tweets-async-timing-check)))
