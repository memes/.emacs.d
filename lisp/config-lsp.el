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
  :commands lsp
  :config
  (progn
    (defun memes/restart-lsp-server ()
      "Restart an LSP server."
      (interactive)
      (lsp-restart-workspace)
      (revert-buffer t t)
      (message "LSP server restarted."))
    (validate-setq lsp-before-save-edits t
                   lsp-inhibit-message t)))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp)

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
