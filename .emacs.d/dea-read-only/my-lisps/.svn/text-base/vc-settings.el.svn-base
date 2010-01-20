;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-19 17:39:24 Thursday by ahei>

(require 'sgml-mode)
(require 'conf-mode)
(require 'image-mode)
(require 'vc-svn)
(require 'log-view)
(require 'vc)
(require 'vc+)

(defun vc-update-and-revert-buffer ()
  (interactive)
  (vc-update)
  (revert-buffer-no-confirm))

(dolist (map (list emacs-lisp-mode-map c-mode-base-map sgml-mode-map
                   sh-mode-map text-mode-map conf-javaprop-mode-map
                   c++-mode-map image-mode-map))
  (apply-define-key
   map 
   `(("C-c l"   vc-print-log)
     ("C-c C-u" vc-update-and-revert-buffer)
     ("C-c C-b" vc-annotate)
     ("C-c C-r" vc-rename)
     ("C-c M-D" vc-delete)
     ("C-c U"   vc-revert)
     ("C-c C-a" svn-add-current-file)
     ("C-c M-r" svn-resolved-current-file)
     ("C-c M-u" vc-update)
     ("C-c M-U" svn-update-current-dir)
     ("C-c v"   svn-status-this-dir-hide)
     ("C-c C-e" vc-ediff-with-prev-rev)
     ("C-c C-v" ,(if is-after-emacs-23 `vc-revision-other-window `vc-version-other-window)))))

(unless is-after-emacs-23
  (setq vc-svn-log-view-mode-map log-view-mode-map))

(apply-define-key
 vc-svn-log-view-mode-map
 '(("u" scroll-down)
   ("SPC" View-scroll-half-page-forward)
   ("Q" kill-this-buffer)
   ("." set-mark-command)
   ("'" switch-to-other-buffer)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)))
