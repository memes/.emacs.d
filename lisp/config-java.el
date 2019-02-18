;;; config-java.el --- Configure java support in Emacs

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package lsp-java
  :ensure t
  :defer t
  :commands lsp-java-enable
  :init
  (after 'config-path
    (setq lsp-java-server-install-dir (memes/lib "eclipse.jdt.ls")))
  :hook
  (java-mode . lsp))

(use-package dap-java
  :after (config-lsp lsp-java))

(provide 'config-java)
;;; config-java.el ends here
