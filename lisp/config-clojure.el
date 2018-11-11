;;; config-clojure.el --- Configures clojure support on Emacs

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (progn
    (define-clojure-indent (gen/let nil))
    (add-to-list 'clojure-align-binding-forms "gen/let")
    (after 'config-completion
      (memes/completion-add-backends 'clojure-mode #'company-capf))
    (after 'paredit
      (add-hook 'clojure-mode-hook #'paredit-mode)))
  :bind
  (:map clojure-mode-map
        ([remap foreward-sexp] . clojure-forward-logical-sexp)
        ([remap backward-sexp] . clojure-backward-logical-sexp)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :after clojure-mode)

(use-package cider
  :ensure t
  :defer t
  :config
  (progn
    (validate-setq cider-font-lock-dynamically '(macro core function deprecated var)
                   cider-font-lock-dynamically nil
                   cider-save-file-on-load t
                   cider-save-files-on-cider-refresh t
                   cider-prompt-for-symbol nil
                   cider-auto-jump-to-error nil
                   cider-prefer-local-resources t
                   cider-dynamic-indentation nil
                   cider-pprint-fn 'pprint))
  :hook
  (((cider-mode cider-repl-mode) . eldoc-mode))
  :bind
  (:map clojure-mode-map
        ("C-c C-t" . cider-test-jump)
        ("C-c C-z" . cider-switch-to-repl-buffer)))

(use-package cider-stacktrace
  :defer t
  :config
  (setq cider-stacktrace-default-filters '(tooling dup java repl)))

(use-package cider-debug
  :defer t
  :config
  (progn
    (defun memes/cider-debug-toggle-eldoc ()
      "Disable eldoc during debugging."
      (if (bound-and-true-p cider--debug-mode)
          (eldoc-mode -1)
        (eldoc-mode 1))))
  :hook
  ((cider--debug-mode-hook . memes/cider-debug-toggle-eldoc)))

(use-package cider-repl
  :defer t
  :config
  (progn
    (after 'config-path
      (setq cider-repl-pop-to-buffer-on-connect nil
            cider-repl-use-clojure-font-lock nil
            cider-repl-use-pretty-printing t
            cider-repl-history-file (memes/var "nrepl-history")
            cider-repl-display-help-banner nil)))
  :bind
  (:map cider-repl-mode-map
        ("C-c C-l" . cider-repl-clear-buffer)))

(provide 'config-clojure)
;;; config-clojure.el ends here
