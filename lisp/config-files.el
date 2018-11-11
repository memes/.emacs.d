;;; config-files.el --- Configurations for backups, dired, etc.

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(setq-default delete-by-moving-to-trash nil
	      default-major-mode 'text-mode
	      large-file-warning-threshold (* 1024 1024))

(use-package recentf
  :defer t
  :init (after-init #'recentf-mode)
  :config
  (validate-setq recentf-max-saved-items 1000
		 recentf-max-menu-items 25))

(use-package tramp-cache
  :defer t
  :config
  (validate-setq tramp-persistency-file-name (memes/var "tramp")))

(use-package autorevert
  :defer t
  :init (after-init #'global-auto-revert-mode)
  :config
  (validate-setq auto-revert-check-vc-info nil))

(use-package simple
  :defer t
  :config
  (validate-setq save-interprogram-paste-before-kill t))

(use-package dired
  :defer t
  :config
  (progn
    (validate-setq dired-auto-revert-buffer t)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode)))

(use-package dired-k
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'dired-initial-position-hook #'dired-k)
    (add-hook 'dired-after-readin-hook #'dired-k-no-revert)))

(use-package vlf-setup
  :ensure vlf)

(provide 'config-files)
;; config-files.el ends here
