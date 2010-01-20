;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-17 23:46:54 Tuesday by ahei>

(setq hide-ifdef-initially t)
(setq hide-ifdef-env
      '((GNU_LINUX . t)
        (__GNUC__ . t)
        (__cplusplus . t)))
(dolist (hook '(c-mode-common-hook))
  (add-hook hook 'hide-ifdef-mode))
(dolist (map (list c-mode-base-map))
  (define-key map (kbd "C-c w") 'hide-ifdef-block)
  (define-key map (kbd "C-c W") 'hide-ifdefs)
  (define-key map (kbd "C-c u") 'show-ifdef-block)
  (define-key map (kbd "C-c U") 'show-ifdefs))

(defun hif-goto-endif ()
  "Goto #endif."
  (interactive)
  (unless (or (hif-looking-at-endif)
              (save-excursion)
    (hif-ifdef-to-endif))))

(defun hif-goto-if ()
  "Goto #if."
  (interactive)
  (hif-endif-to-ifdef))

(defun hif-gototo-else ()
  "Goto #else."
  (hif-find-next-relevant)
  (cond ((hif-looking-at-else)
         'done)
	 (hif-ifdef-to-endif) ; find endif of nested if
	 (hif-ifdef-to-endif)) ; find outer endif or else
	((hif-looking-at-else)
	 (hif-ifdef-to-endif)) ; find endif following else
	((hif-looking-at-endif)
	 'done)
	(t
	 (error "Mismatched #ifdef #endif pair")))


(defun hif-find-next-relevant ()
  "Move to next #if..., #else, or #endif, after the current line."
  ;; (message "hif-find-next-relevant at %d" (point))
  (end-of-line)
  ;; avoid infinite recursion by only going to beginning of line if match found
  (re-search-forward hif-ifx-else-endif-regexp (point-max) t))
