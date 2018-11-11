;;; config-lsp.el --- Configuration for LSP support in Emacs

;;

;;; Commentary:
;;
;; Loads common lsp (language server protocol) support into Emacs session
;;

;;; Code:
(require 'use-config)

(use-package lsp-mode
  :ensure t
  :defer t
  :config
  (progn
    (defun memes/lsp-set-projectile-root ()
      (when lsp--cur-workspace
        (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
    (defun memes/restart-lsp-server ()
      "Restart an LSP server."
      (interactive)
      (lsp-restart-workspace)
      (revert-buffer t t)
      (message "LSP server restarted."))
    (validate-setq lsp-before-save-edits t
                   lsp-inhibit-message t)
    (after 'projectile
      (add-hook 'lsp-before-open-hook #'memes/lsp-set-projectile-root)))
  :hook
  (lsp-after-open . lsp-enable-imenu))

(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (validate-setq lsp-ui-sideline-enable t
                 lsp-ui-doc-enable nil
                 lsp-ui-flycheck-enable t
                 lsp-ui-imenu-enable nil
                 lsp-ui-sideline-ignore-duplicate t)
  :hook
  (lsp-mode . lsp-ui-mode))

;; Make company-lsp available, but enable on a per-language basis so it can be
;; controlled for buggy implementations
;;
;; E.g. (use-package lang-impl
;;        ...
;;        :config
;;        (memes/completion-add-backends 'lang-mode 'company-lsp))
(use-package company-lsp
  :ensure t
  :defer t)

;; Enable debugger protocol integration
;; - language support (Java/Python currently) must be enable in the
;;   respective language config.
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(provide 'config-lsp)
;;; config-lsp.el ends here
