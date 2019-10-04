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
  :commands (lsp lsp-deferred)
  :config
  (progn
    (defun memes/restart-lsp-server ()
      "Restart an LSP server."
      (interactive)
      (lsp-workplace-restart)
      (revert-buffer t t)
      (message "LSP server restarted."))
    (validate-setq lsp-before-save-edits t
                   lsp-prefer-flymake nil
                   lsp-auto-guess-root t)))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode
  :config
  (validate-setq lsp-ui-sideline-enable nil
                 lsp-ui-doc-enable nil))

(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp
  :config
  (validate-setq company-lsp-enable-snippet t
                 company-idle-delay .2
                 company-tooltip-limit 20))

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
