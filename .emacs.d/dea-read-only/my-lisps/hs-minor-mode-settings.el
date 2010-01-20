;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-21 22:05:25 Saturday by ahei>

(require 'hideshow)

(defvar hs-headline-max-len 30 "*Maximum length of `hs-headline' to display.")

(setq hs-isearch-open t)

(defun hs-display-headline ()
  (let* ((len (length hs-headline))
         (headline hs-headline)
         (postfix ""))
    (when (>= len hs-headline-max-len)
      (setq postfix "...")
      (setq headline (substring hs-headline 0 hs-headline-max-len)))
    (if hs-headline (concat headline postfix " ") "")))

(setq-default mode-line-format
              (append '((:eval (hs-display-headline))) mode-line-format))

(dolist (hook (list 'c-mode-common-hook 'lisp-mode-hook 'emacs-lisp-mode-hook 'java-mode-hook))
  (add-hook hook 'hs-minor-mode))
(dolist (map (list c-mode-base-map emacs-lisp-mode-map))
  (define-key map (kbd "C-c h") 'hs-hide-block)
  (define-key map (kbd "C-c H") 'hs-hide-all)
  (define-key map (kbd "C-c e") 'hs-show-block)
  (define-key map (kbd "C-c E") 'hs-show-all))
(setq hs-set-up-overlay 'hs-abstract-overlay)

(defun hs-abstract-overlay (ov)
  (let ((str (format "[该块%d行已被折叠]" (count-lines (overlay-start ov) (overlay-end ov)))) text)
    (if window-system
        (setq text (propertize str 'face 'yellow-forestgreen-face))
      (setq text (propertize str 'face 'white-red-face)))
    (overlay-put ov 'display text)))
