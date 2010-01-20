;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-12-25 00:16:11 Friday by ahei>

(require 'w3m)
(require 'w3m-lnum)

(defvar w3m-buffer-name-prefix "*w3m" "Name prefix of w3m buffer")
(defvar w3m-buffer-name (concat w3m-buffer-name-prefix "*") "Name of w3m buffer")
(defvar w3m-bookmark-buffer-name (concat w3m-buffer-name-prefix "-bookmark*") "Name of w3m buffer")

(setq w3m-command-arguments '("-cookie" "-F"))
(setq w3m-use-cookies t)
(setq w3m-use-toolbar t)
(setq w3m-use-mule-ucs t)
(setq w3m-home-page "http://www.google.cn")
(setq w3m-default-display-inline-images t)

(defun w3m-settings ()
  (make-local-variable 'hl-line-face)
  (setq hl-line-face 'hl-line-nonunderline-face)
  (setq hl-line-overlay nil)
  (color-theme-adjust-hl-line-face))

(add-hook 'w3m-mode-hook 'w3m-settings)

(defun w3m-save-current-buffer ()
  "Save current w3m buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (call-interactively 'copy-region-as-kill-nomark))
  (with-temp-buffer
    (call-interactively 'yank)
    (call-interactively 'write-file)))

(defun w3m-print-current-url ()
  "Display current url."
  (interactive)
  (w3m-message "%s" (w3m-url-readable-string w3m-current-url)))

(defun w3m-copy-current-url ()
  "Display the current url in the echo area and put it into `kill-ring'."
  (interactive)
  (when w3m-current-url
    (let ((deactivate-mark nil))
      (kill-new w3m-current-url)
      (w3m-print-current-url))))

(defun view-w3m-bookmark ()
  "View w3m bokmark."
  (interactive)
  (let ((buffer (get-buffer w3m-bookmark-buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (with-current-buffer (get-buffer-create w3m-bookmark-buffer-name)
        (w3m-mode)
        (w3m-bookmark-view)))))

(defun switch-to-w3m ()
  "Switch to *w3m* buffer."
  (interactive)
  (let ((buffer (get-buffer w3m-buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (message "Could not found w3m buffer."))))

(defun w3m-browse-current-buffer ()
  "Use w3m browser current buffer."
  (interactive)
  (w3m-browse-buffer))

(defun w3m-browse-buffer (&optional buffer)
  "Use w3m browser buffer BUFFER."
  (interactive "bBuffer to browse use w3m: ")
  (unless buffer (setq buffer (current-buffer)))
  (let* ((file (buffer-file-name buffer))
         (name (buffer-name buffer)))
    (if file
        (w3m-goto-url-new-session file)
      (with-current-buffer buffer
        (save-excursion
          (mark-whole-buffer)
          (call-interactively 'copy-region-as-kill-nomark)))
      (let* ((new-name
              (concat
               w3m-buffer-name-prefix
               "-"
               (if (string= "*" (substring name 0 1))
                   (substring name 1)
                 (concat name "*"))))
             (new-buffer (get-buffer-create new-name)))
        (switch-to-buffer new-buffer)
        (call-interactively 'yank)
        (w3m-buffer)
        (w3m-mode)
        (setq w3m-current-title (buffer-name))))))

;; fix small bug about of `w3m-auto-show'
;; see my-blog/emacs/w3m-auto-show-bug.htm
(defun w3m-auto-show ()
  "Scroll horizontally so that the point is visible."
  (when (and truncate-lines
             w3m-auto-show
             (not w3m-horizontal-scroll-done)
             (not (and (eq last-command this-command)
                       (or (eq (point) (point-min))
                           (eq (point) (point-max)))))
             (or (memq this-command '(beginning-of-buffer end-of-buffer))
                 (string-match "\\`i?search-"
                               (if (symbolp this-command) (symbol-name this-command) ""))
                 (and (markerp (nth 1 w3m-current-position))
                      (markerp (nth 2 w3m-current-position))
                      (>= (point)
                          (marker-position (nth 1 w3m-current-position)))
                      (<= (point)
                          (marker-position (nth 2 w3m-current-position))))))
    (w3m-horizontal-on-screen))
  (setq w3m-horizontal-scroll-done nil))

(apply-define-key
 global-map
 `(("M-M"     w3m-goto-url-new-session)
   ("C-x M-B" view-w3m-bookmark)
   ("C-x M-m" switch-to-w3m)))

(apply-define-key
 w3m-mode-map
  `(("<backtab>" w3m-previous-anchor)
    ("n"         w3m-next-anchor)
    ("p"         w3m-previous-anchor)
    ("w"         w3m-next-form)
    ("b"         w3m-previous-form)
    ("f"         w3m-go-to-linknum)
    ("M-n"       w3m-next-buffer)
    ("M-p"       w3m-previous-buffer)
    ("C-k"       kill-this-buffer)
    ("C-k"       w3m-delete-buffer)
    ("C-c 1"     w3m-delete-other-buffers)
    ("1"         delete-other-windows)
    ("C-x C-s"   w3m-save-current-buffer-sb)
    ("P"         w3m-print-current-url)
    ("U"         w3m-print-this-url)
    ("c"         w3m-copy-current-url)
    ("g"         w3m-goto-url-new-session)
    ("G"         w3m-goto-url)
    ("d"         w3m-download-this-url-sb)
    ("M-d"       w3m-download-sb)
    ("s"         w3m-search)
    ("S"         w3m-history)
    ("u"         View-scroll-page-backward)
    ("J"         roll-down)
    ("K"         roll-up)
    ("o"         other-window)
    ("m"         w3m-view-this-url-new-session)
    ("C-h"       w3m-view-previous-page)
    ("F"         w3m-view-next-page)
    ("C-;"       w3m-view-next-page)
    ("r"         w3m-reload-this-page)
    ("v"         w3m-bookmark-view-new-session)
    ("M-e"       w3m-bookmark-edit)
    ("'"         switch-to-other-buffer)))
