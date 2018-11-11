;;; config-markdown.el --- Configure Markdown support and preview in Emacs

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (after 'config-completion
    (memes/completion-add-backends 'markdown-mode 'company-capf))
  :hook
  (markdown-mode . memes/enable-mmm-mode))

(use-package company-emoji
  :defer t
  :ensure t
  :config
  (after 'config-completion
    (memes/completion-add-backends 'markdown-mode 'company-emoji)))

(use-package emoji-cheat-sheet-plus
  :defer t
  :ensure t
  :config
  (after 'config-completion
    (memes/completion-add-backends 'markdown-mode 'emoji-cheat-sheet-plus-display-mode)))

(use-package gh-md
  :ensure t
  :defer t)

(use-package markdown-toc
  :ensure t
  :defer t)

;;;###autoload
(defun memes/add-markdown-lang-mode (lang &optional mode)
  "Define and register a mmm-mode class for `LANG` in `markdown-mode` using `MODE`.
If `MODE` is not provided, use `LANG-mode` as the default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or mode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```$"))
    (mmm-add-classes (list (list class
                                 :submode submode
                                 :front front
                                 :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

(provide 'config-markdown)
;;; config-markdown.el ends here
