;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-07 01:33:11 Monday by ahei>

(require 'cc-mode)

;; cedet1.0pre6 is conflict with which-func
;; after require cedet, which-func cann't work
(require 'cedet)
(require 'semantic-ia)
(require 'semantic-gcc)
(require 'semanticdb-global)
(require 'semantic-ectag-util)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
(when window-system
  (global-semantic-tag-folding-mode 1))

;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

(cogre-uml-enable-unicode)

;; TODO: 怎样可以不用这样取消`senator-prefix-key'的prefix command
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (make-local-variable 'senator-prefix-key)
;;              (setq senator-prefix-key nil)) t)
(dolist (map (list c-mode-base-map emacs-lisp-mode-map))
  (define-key map (kbd "C-c j") 'semantic-complete-jump-local)
  (define-key map (kbd "C-c n") 'senator-next-tag)
  (define-key map (kbd "C-c p") 'senator-previous-tag))

;; system include path
(if (or mswin cygwin)
    (dolist (mode '(c-mode c++-mode))
      (semantic-add-system-include "c:/cygwin/usr/include/" mode)))

(unless mswin
  ;; if you want to enable support for gnu global
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
(when (semantic-ectag-version)
  (semantic-load-enable-primary-exuberent-ctags-support))
