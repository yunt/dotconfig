;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-18 13:09:41 Wednesday by ahei>

(require 'linkd)

(setq linkd-use-icons t)
(setq linkd-icons-directory (concat my-emacs-lisps-path "linkd/icons"))

(dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook))
  (add-hook hook
            '(lambda ()
               (linkd-mode 1)
               (linkd-enable))))

(apply-define-key
 linkd-overlay-map
 `(("n"        linkd-next-link)
   ("p"        linkd-previous-link)
   ("<return>" linkd-follow-at-point)))

(apply-define-key
 linkd-map
 `(("<mouse-4>" nil)
   ("C-c ," nil)))
