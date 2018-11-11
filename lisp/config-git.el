;;; config-git.el --- Emacs git configuration

;;

;;; Commentary:

;;

;;; Code:
(use-package vc
  :defer t
  :config
  (validate-setq vc-handled-backends '(Git)
		 vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
					      vc-ignore-dir-regexp
					      tramp-file-name-regexp)))
(use-package ediff
  :defer t
  :config
  (validate-setq ediff-highlight-all-diffs nil
		 ediff-window-setup-function 'edif-setup-windows-plain
		 ediff-diff-options "-w"
		 ediff-split-window-function 'split-window-horizontally))

(use-package magit
  :ensure t
  :defer t)

(use-package magit-gh-pulls
  :ensure t
  :after magit
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package gited
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package diff-hl
  :ensure t
  :defer t
  :commands diff-hl-mode
  :init
  (progn
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode))
  :config
  (progn
    (validate-setq diff-hl-draw-borders t)
    (diff-hl-margin-mode 1)))

(use-package gitattributes-mode
  :ensure t
  :defer t
  :mode (("/\\.gitattributes\\'" . gitattributes-mode)
         ("/info/attributes\\'" . gitattributes-mode)
         ("/git/attributes\\'" . gitattributes-modes)))

(use-package gitconfig-mode
  :ensure t
  :defer t
  :mode (("/\\.gitconfig\\'" . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/modules/.*/config\\'" . gitconfig-mode)
         ("/git/config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/etc/gitconfig\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :ensure t
  :defer t
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

  (provide 'config-git)
;;; config-git.el ends here
