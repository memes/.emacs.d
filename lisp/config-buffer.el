;;; config-buffer.el --- Setup buffer display, mode-line, etc.

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(validate-setq window-resize-pixelwise t)

;; Setup the modeline
(use-package simple
  :config
  (progn
    (line-number-mode 1)
    (setq display-time-day-and-date nil)))

(use-package fringe
  :init (fringe-mode 4)
  :config
  (progn
    (validate-setq indicate-empty-lines t
		   indicate-buffer-boundaries t
		   indicate-unused-lines t)
    (setf (cdr (assq 'continuation fringe-indicator-alist))
	  '(nil right-curly-arrow))))

(use-package hl-line
  :defer t
  :init
  (progn
    (add-hook 'dired-mode-hook #'hl-line-mode)
    (after 'magit (add-hook 'magit-mode-hook #'hl-line-mode)))
  :config
  (validate-setq global-hl-line-sticky-flag nil
		 hl-line-sticky-flag nil))

(use-package rich-minority
  :ensure t
  :init (rich-minority-mode 1)
  :config
  (validate-setq rm-whitelist '(" λ")))

(use-package focus
  :ensure t
  :defer t)

;; Utility functions for manipulating buffers

;; Return a list of temp buffers
(defun memes/list-temp-buffers ()
  "Return a list of temp buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (string-match-p "\\*temp" (buffer-name buffer)))
   (buffer-list)))

;; Interactively create or switch to temp buffer
;;;###autoload
(defun memes/temp-buffer (arg)
  "Create or switch to *temp* buffer.
When called with `ARG` always create a new temp buffer."
  (interactive "P")
  (let* ((n (if (equal '(4) arg) (length (memes/list-temp-buffers)) 0))
         (name (format "*temp%s" (if (>= 0 n) "*" (format "-%s*" n))))
         (buffer (get-buffer-create name))
         (mode major-mode))
    (with-current-buffer buffer
      (funcall mode)
      (switch-to-buffer buffer))))

;; Interactively switch to last buffer
;;;###autoload
(defun memes/switch-to-last-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

;; Quickly revert a file, bound to Alt-r
;;;###autoload
(defun memes/revert-buffer-noconfirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(provide 'config-buffer)
;;; config-buffer.el ends here
