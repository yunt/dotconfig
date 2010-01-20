;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-02 22:48:12 Wednesday by ahei>

(require 'highlight-symbol)
(require 'sgml-mode)

(when window-system
  (setq highlight-symbol-idle-delay 0.5)
  (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook java-mode-hook
                                       c-mode-common-hook text-mode-hook ruby-mode-hook html-mode-hook))
    (add-hook hook 'highlight-symbol-mode-on)))

(defun highlight-symbol-mode-on ()
  "Turn on function `highlight-symbol-mode'."
  (highlight-symbol-mode 1))

(defun highlight-symbol-mode-off ()
  "Turn off function `highlight-symbol-mode'."
  (highlight-symbol-mode -1))

;;;###autoload
(define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode highlight-symbol-mode-on)

;; I bind "C-x w" to `copy-sexp'
(apply-define-key
 hi-lock-map
 `(("C-x w" nil)))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map java-mode-map
                   c-mode-base-map text-mode-map ruby-mode-map html-mode-map))
  (apply-define-key
   map
   `(("C-c M-H" highlight-symbol-at-point)
     ("C-c M-R" highlight-symbol-remove-all)
     ("C-c M-N" highlight-symbol-next)
     ("C-c M-P" highlight-symbol-prev)
     ("C-c r"   highlight-symbol-query-replace)
     ("C-c M-n" highlight-symbol-next-in-defun)
     ("C-c M-p" highlight-symbol-prev-in-defun))))
