;;; config-javascript.el --- Configure javascript and related language support

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defconst memes/js-indent 2
  "Default indentation for javascript and related modes.")

(use-package js2-mode
  :ensure t
  :defer t
  :mode
  (("\\.js\\'" . js2-mode)
   ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter
  (("node" . js2-mode)
   ("node" . js2-jsx-mode))
  :commands flycheck-javascript-eslint-executable
  :init
  :config
  (progn
    (after (flycheck)
      (if (or (executable-find "eslint_d")
              (executable-find "eslint")
              (executable-find "jshint"))
          (validate-setq js2-mode-show-strict-warnings nil))
      (if (executable-find "eslint_d")
          (validate-setq flycheck-javascript-eslint-executable "eslint_d")))
    (after 'javascript-mode
      (validate-setq js-indent-level memes/js-indent
                     indent-tabs-mode nil))
    (validate-setq js2-basic-offset memes/js-indent
                   js2-bounce-indent-p t)
    (use-package js2-refactor
      :diminish js2-refactor-mode
      :config
      (js2r-add-keybindings-with-prefix "C-c C-m")
      :hook
      (js2-mode . js2-refactor-mode)))
  :hook
  ((js2-mode . js2-imenu-extras-mode)
   (js2-mode . js2-highlight-unused-variables-mode)))

(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (validate-setq typescript-indent-level memes/js-indent))

(use-package lsp-javascript-typescript
  :ensure t
  :defer t
  :commands lsp-javascript-typescript-enable
  :init
  (progn
    (after (js2-mode config-completion config-lsp)
      (memes/completion-add-backends 'js2-mode 'company-lsp))
    (after (typescript-mode config-completion config-lsp)
      (memes/completion-add-backends 'typescript-mode 'company-lsp)))
  :hook
  ((typescript-mode js2-mode) . lsp-javascript-typescript-enable))

(use-package mocha
  :ensure t
  :defer t
  :config
  (use-package mocha-snippets))

(use-package coffee-mode
  :ensure t
  :defer t
  :config
  (validate-setq coffee-tab-width memes/js-indent))

(provide 'config-javascript)
;;; config-javascript.el ends here
