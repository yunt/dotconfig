;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-05 00:40:17 Saturday by ahei>

(require 'autopair)

;; After do this, isearch any string, M-: (match-data) always return (0 3)
(autopair-global-mode 1)

(setq autopair-extra-pairs `(:everywhere ((?` . ?'))))

(defun autopair-insert-opening-internal ()
  (interactive)
  (when (autopair-pair-p)
    (setq autopair-action (list 'opening (autopair-find-pair) (point))))
  (autopair-fallback))

(defun autopair-insert-opening ()
  (interactive)
  (if (and (memq major-mode '(c-mode c++-mode java-mode)) (equal last-command-event ?{))
      (call-interactively 'skeleton-c-mode-left-brace)
    (call-interactively 'autopair-insert-opening-internal)))

(apply-define-key
 global-map
 `(("C-h" delete-backward-char)))
