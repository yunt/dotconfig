;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-19 15:14:32 Thursday by ahei>

(require 'palette)

(defun blink-cursor-mode-disable ()
  "Disable `blink-cursor-mode'."
  (interactive)
  (blink-cursor-mode -1))

(add-hook 'palette-mode-hook 'blink-cursor-mode-disable)

(defmacro def-palette-move-command (move-command speed)
  "Make definition of command which palette move quickly."
  `(defun ,(concat-name move-command "-quickly") ()
     ,(concat move-command " quickly.")
     (interactive)
     (,(symbol-function (intern move-command)) ,speed)))

(apply-args-list-to-fun
 'def-palette-move-command
 `(("palette-down"  7)
   ("palette-up"    7)
   ("palette-left"  7)
   ("palette-right" 7)))

(apply-define-key
 palette-mode-map
 `(("j"     palette-down)
   ("k"     palette-up)
   ("h"     palette-left)
   ("l"     palette-right)
   ("J"     palette-down-quickly)
   ("K"     palette-up-quickly)
   ("H"     palette-left-quickly)
   ("L"     palette-right-quickly)
   ("r"     palette-face-restore-bg-fg)
   ("f"     palette-set-face-changed-to-foreground)
   ("b"     palette-set-face-changed-to-background)
   ("B"     facemenup-face-bg-restore)
   ("F"     facemenup-face-fg-restore)
   ("d"     palette-disply-which-in-changine)
   ("m"     palette-pick-background-at-point)
   ("C"     palette-copy-current-color)
   ("C-x k" palette-quit-restore-bg-fg)))

(defun kill-palette-buffers (frame)
  (when (get-buffer "Palette (Hue x Saturation)") (kill-buffer "Palette (Hue x Saturation)"))
  (when (get-buffer "Brightness") (kill-buffer "Brightness"))
  (when (get-buffer "Current/Original") (kill-buffer "Current/Original")))

(add-to-list 'delete-frame-functions 'kill-palette-buffers)

(if window-system
    (let ((font (car (x-list-fonts "-outline-Courier New-normal-normal-normal-mono-6-*-*-*-c-*-iso8859-1" nil nil 1))))
      (if font
          (setq palette-font font))))

(apply-define-key
 global-map
 `(("C-x M-F" facemenup-palette-face)
   ("C-x M-P" facemenup-palette-face-at-point)))

(defvar facemenup-palette-change-face-bg nil "Face background be changed or not.")
(defvar facemenup-last-face-color nil "Last face color used.")

(defun palette-quit-restore ()
  "Quite palette and restore face changed."
  (interactive)
  (palette-quit)
  (palette-face-restore))

(defun palette-quit-restore-bg-fg ()
  "Quite palette and restore face changed."
  (interactive)
  (palette-quit)
  (palette-face-restore-bg-fg))

(defun palette-face-restore ()
  "Restore face have changed."
  (interactive)
  (unless (or (string= facemenup-last-face-color palette-current-color)
              (string= (hexrgb-color-name-to-hex facemenup-last-face-color) palette-current-color))
    (if facemenup-palette-change-face-bg
        (facemenup-face-bg-restore)
      (facemenup-face-fg-restore))))

(defun palette-face-restore-bg-fg ()
  "Restore face background and foreground have changed."
  (interactive)
  (facemenup-face-bg-restore)
  (facemenup-face-fg-restore))

(defun facemenup-palette-face-at-point (&optional is-bg)
  "Open palette and set face at point to face which to be changed."
  (interactive "P")
  (let (face (hl-line hl-line-mode))
    (if hl-line
        (hl-line-mode -1))
    (setq face (eyedrop-face-at-point))
    (if hl-line
        (hl-line-mode 1))
    (if face
        (facemenup-palette-face face is-bg)
      (call-interactively 'facemenup-palette-face))))

(defun facemenup-palette-face (face &optional is-bg)
  "Open palette and set face FACE to face which to be changed."
  (interactive
   (list
    (read-face-name
     (concat "Face whose " (if current-prefix-arg "background" "foreground") " to change"))
    current-prefix-arg))
  (setq facemenup-palette-change-face-bg is-bg)
  (let ((color
         (if is-bg
             (facemenup-face-bg face)
           (facemenup-face-fg face))))
    (setq facemenup-last-face-bg (facemenup-face-bg face))
    (setq facemenup-last-face-fg (facemenup-face-fg face))
    (setq facemenup-last-face-color color)
    (setq facemenup-last-face-changed face)
    (when facemenup-palette-update-while-editing-flag
      (palette-set-face-changed-to-foreground is-bg))
    (condition-case nil
        (palette color)
      (quit (set-face-foreground face color)))))

(defun palette-set-face-changed-to-foreground (&optional is-bg)
  "Set face change color to foreground."
  (interactive "P")
  (setq facemenup-palette-change-face-bg is-bg)
  (setq palette-action
        `(lambda ()
           ,(if is-bg
                `(set-face-background facemenup-last-face-changed palette-current-color)
              `(set-face-foreground facemenup-last-face-changed palette-current-color))))
  (message (concat "Change to " (if is-bg "back" "fore") "ground")))

(defun palette-set-face-changed-to-background (&optional is-fg)
  "Set face change color to background."
  (interactive "P")
  (palette-set-face-changed-to-foreground (not is-fg)))

(defun palette-disply-which-in-changine ()
  "Display which face in changing on palette."
  (interactive)
  (message (concat "Color changing is: %s's " (if facemenup-palette-change-face-bg "back" "fore") "ground") facemenup-last-face-changed))
(defun palette-copy-current-color ()
  "Copy current color."
  (interactive)
  (kill-new palette-current-color)
  (message "Color %s copied." palette-current-color))
