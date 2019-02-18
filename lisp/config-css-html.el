;;; config-css-html.el --- Configure Emacs for CSS and HTML editing

;;

;;; Commentary:

;; npm install -g vscode-css-languageserver-bin vscode-html-languageserver-bin

;;

;;; Code:
(require 'use-config)

(defcustom memes/default-web-indent 2
  "A default value to use when indenting files for web development."
  :type 'integer
  :group 'memes/css-html)

(use-package css-mode
  :ensure nil
  :defer t
  :config
  (progn
    (validate-setq css-indent-offset memes/default-web-indent)
    (after 'rainbow-mode
      (add-hook 'css-mode-hook #'rainbow-mode)))
  :hook
  (css-mode . lsp))

(use-package sass-mode
  :ensure t
  :defer t
  :config
  (progn
    (validate-setq sass-indent-offset memes/default-web-indent)
    (after 'rainbow-mode
      (add-hook 'sass-mode-hook #'rainbow-mode)))
  :hook
  (sass-mode . lsp))

(use-package scss-mode
  :ensure t
  :defer t
  :config
  (progn
    (validate-setq scss-compile-at-save nil)
    (after 'rainbow-mode
      (add-hook 'scss-mode-hook #'rainbow-mode)))
  :hook
  (scss-mode . lsp))

(use-package less-css-mode
  :ensure t
  :defer t
  :config
  (progn
    (validate-setq less-css-compile-at-save nil)
    (after 'rainbow-mode
      (add-hook 'less-mode #'rainbow-mode)))
  :hook
  (less-mode . lsp))

(use-package css-eldoc
  :ensure t
  :defer t
  :commands turn-on-css-eldoc
  :hook
  ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (progn
    (after 'aggressive-indent
      (add-hook 'web-mode-hook #'aggressive-indent-mode))
    (after 'paredit
      (add-hook 'web-mode-hook #'paredit-mode))
    (after 'rainbow-mode
      (add-hook 'web-mode-hook 'rainbow-mode))
    (validate-setq web-mode-markup-indent-offset memes/default-web-indent
		   web-mode-css-indent-offset memes/default-web-indent
		   web-mode-code-indent-offset memes/default-web-indent
		   web-mode-enable-auto-pairing t
		   web-mode-enable-css-colorization t
		   web-mode-enable-current-element-highlight t
		   web-mode-enable-current-column-highlight t
		   web-mode-enable-engine-detection t))
  :hook
  ((html-mode . lsp)
   (web-mode . lsp)))

(provide 'config-css-html)
;;; config-css-html.el ends here
