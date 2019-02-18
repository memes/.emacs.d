;;; config-projects.el --- Emacs project configuration

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package projectile
  :ensure t
  :defer t
  :init (after-init #'projectile-mode)
  :commands projectile-mode
  :functions (projectile-load-known-projects)
  :config
  (progn
    (after 'ivy
      (setq projectile-completion-system 'ivy))
    (validate-setq projectile-enable-caching nil
		   projectile-cache-file (memes/var "projectile.cache")
		   projectile-known-projects-file (memes/var "projectile-bookmarks.eld")
		   projectile-use-git-grep t
		   projectile-create-missing-test-files t
		   projectile-globally-ignored-directories (append projectile-globally-ignored-directories '("elpa")))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-hook 'projectile-idle-timer-hook #'projectile-invalidate-cache)
    (advice-add #'projectile-replace :before #'projectile-save-project-buffers)
    (projectile-load-known-projects)
    (projectile-mode +1)))

(provide 'config-projects)
;;; config-projects.el ends here
