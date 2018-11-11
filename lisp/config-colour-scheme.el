;;; config-colour-scheme.el --- colour scheme for Emacs

;;; Commentary:

;;; Code:
(require 'use-config)

(defcustom memes/theme 'solarized-dark
  "Default theme."
  :type 'symbol
  :group 'colour-scheme)

(use-package solarized-theme
  :ensure t
  :defer t)

;; Function load a theme and turn on font decorations
(defun memes/load-theme ()
  "Load my current theme."
  (when memes/theme
    (load-theme memes/theme t)))

(use-package custom
  :defer t
  :commands load-theme
  :init
  (progn
    (setq custom-theme-directory (let ((theme-dir (expand-file-name "themes/" user-emacs-directory)))
				   (unless (file-exists-p theme-dir)
				     (make-directory theme-dir 'parents))
				   theme-dir))
    (memes/add-load-path custom-theme-directory)
    (after-init #'memes/load-theme)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :commands rainbow-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package page-break-lines
  :ensure t
  :defer t
  :commands (page-break-lines-mode)
  :functions (page-break-lines--update-display-tables)
  :init (add-hook 'prog-mode-hook #'page-break-lines-mode)
  :preface
  (defun memes/set-page-break-line-height ()
    "Fix interaction with company overlay.
  https://github.com/purcell/page-break-lines/issues/3"
    (set-fontset-font
     "fontset-default"
     (cons page-break-lines-char page-break-lines-char)
     (face-attribute 'default :family))
    (set-face-attribute
     'page-break-lines nil
     :height (face-attribute 'default :height nil 'default)))
  (defun memes/page-break-over-tooltip-p ()
    (when (boundp 'company-tooltip-limit)
      (let ((to (save-excursion (forward-line company-tooltip-limit) (point))))
        (save-excursion
          (numberp (re-search-forward "" to t))))))
  (defun memes/maybe-turn-off-page-breaks (&optional _)
    (when (and (bound-and-true-p page-break-lines-mode)
               (memes/page-break-over-tooltip-p))
      (put 'page-break-lines-mode :config-toggle t)
      (page-break-lines-mode -1)))
  (defun memes/maybe-turn-on-page-breaks (&optional _)
    (unless (or (bound-and-true-p page-break-lines-mode)
                (null (get 'page-break-lines-mode :config-toggle)))
      (put 'page-break-lines-mode :config-toggle nil)
      (page-break-lines-mode 1)))
  :config
  (progn
    (add-hook 'page-break-lines-mode-hook #'memes/set-page-break-line-height)
    ;; Fix wrapping when switching buffers
    (advice-add
     #'switch-to-buffer :after
     (lambda (_ &optional no-confirm no-enable) (page-break-lines--update-display-tables)))
    ;; Disable with company popup
    (after 'company
      (add-hook 'company-completion-started-hook #'memes/maybe-turn-off-page-breaks)
      (add-hook 'company-completion-finished-hook #'memes/maybe-turn-on-page-breaks)
      (add-hook 'company-completion-cancelled-hook #'memes/maybe-turn-on-page-breaks))))

(use-package hl-todo
  :ensure t
  :defer t
  :commands hl-todo-mode
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(provide 'config-colour-scheme)
;;; config-colour-scheme.el ends here
