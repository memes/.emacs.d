;;; config-lisp.el --- Configuration for lisp mode(s)

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)
(require 'map)

(use-package elisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-mode))

(after (simple paredit)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(after 'flycheck
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :after lisp
  :config (lisp-extra-font-lock-global-mode 1))

(provide 'config-lisp)
;; config-lisp.el ends here
