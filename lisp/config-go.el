;;; config-go.el --- Configure Emacs for go support

;;

;;; Commentary:

;; go get -u golang.org/x/tools/cmd/gopls

;;; Code:
(require 'use-config)

(defvar memes/go-lsp nil
  "Enable LSP backend for go-mode when non-nil.")

(use-package go-mode
  :ensure t
  :defer t
  :init
  (defun memes/go-pkgs()
    "Return a list of all GO packages, using `gopkgs`."
    (sort (process-lines "gopkgs") #'string<))
  (defun memes/go-compile ()
    "Return the commands to compile current project."
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  (if memes/go-lsp
      (add-hook 'go-mode-hook #'lsp))
  :config
  (validate-setq gofmt-command "goimports"
                 go-packages-function 'memes/go-pkgs)
  :hook
  ((before-save . gofmt-before-save)
   (go-mode . memes/go-compile))
  :bind
  (:map go-mode-map
        ([remap xref-find-definitions] . godef-jump)
        ("C-c r" . go-remove-unused-imports)
        ("C-c i" . go-goto-imports)))

;; If not using LSP mode
(use-package company-go
  :if (not memes/go-lsp)
  :ensure t
  :defer t
  :init
  (after 'config-completion
    (memes/completion-add-backends 'go-mode 'company-go)))

;; Make go-mode aware of projectile
(use-package go-projectile
  :ensure t
  :defer t
  :after projectile
  :commands (go-projectile-mode go-projectile-switch-project)
  :hook
  ((go-mode . go-projectile-mode)
   (projecile-after-switch-project . go-projectile-switch-project)))


(use-package go-eldoc
  :ensure t
  :defer t
  :after go-mode
  :commands go-eldoc-setup
  :hook
  (go-mode . go-eldoc-setup)
  :config
  (if (file-exists-p (concat (projectile-project-root) "go.mod"))
      (validate-setq go-eldoc-gocode (expand-file-name "go/bin/gocode-gomod" memes/local-libs-root))
    (validate-setq go-eldoc-gocode (expand-file-name "go/bin/gocode" memes/local-libs-root))))

;; Guru
(use-package go-guru
  :ensure t
  :defer t
  :after go-mode
  :bind
  (:map go-mode-map
        ([remap xref-find-references] . go-guru-referrers)))

;; Allow godoctor for refactoring
(use-package godoctor
  :ensure t
  :defer t)

;; Prefer golangci's metalinter
(use-package flycheck-golangci-lint
  :ensure t
  :defer t
  :init
  (after 'flycheck
    (validate-setq flycheck-disabled-checkers '(go-fmt
                                                go-golint
                                                go-vet
                                                go-build
                                                go-test
                                                go-errcheck)))
  :hook
  (go-mode . flycheck-golangci-lint-setup))

;; Integrate with delve
(use-package go-dlv
  :ensure t
  :defer t)

;; Renamer
(use-package go-rename
  :ensure t
  :defer t
  :bind
  (:map go-mode-map
        ("C-c R" . go-rename)))


(use-package go-impl
  :ensure t
  :defer t
  :after go-mode)

(use-package go-fill-struct
  :ensure t
  :defer t)

(use-package go-playground
  :ensure t
  :defer t
  :commands go-playground-mode)

(use-package gotest
  :ensure t
  :defer t
  :commands go-test-current-project go-test-current-file go-test-current-test go-run
  :config
  (after 'popwin
    (push (cons "*Go Test*" '(:dedicated t :position bottom :stick t :noselect 4 :height 0.4)) popwin:special-display-config))
  :bind
  (:map go-mode-map
        ("C-c p" . go-test-current-project)
        ("C-c f" . go-test-current-file)
        ("C-c t" . go-test-current-test)
        ("C-c x" . go-run)))

(use-package go-gen-test
  :ensure t
  :defer t
  :commands go-gen-test-dwim
  :bind
  (:map go-mode-map
        ("C-c C-t" . go-gen-test-dwim)))

(provide 'config-go)
;;; config-go.el ends here
