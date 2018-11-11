;;; config-go.el --- Configure Emacs for go support

;;

;;; Commentary:

;; go get -u github.com/sourcegraph/go-langserver

;;; Code:
(require 'use-config)

;; Do I even need this anymore?
(defconst memes/goroot
  (convert-standard-filename (expand-file-name
                              (cond ((string-equal system-type "darwin") "~/Library/go")
                                    (t "~/lib/go"))))
  "Local GOROOT customisation based on OS.")

(use-package go-mode
  :ensure t
  :defer t
  :init
  (defun memes/go-compile ()
    "Return the commands to compile current project."
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "gometalinter --deadline 10s && go build -v && go test -v && go vet")))
  :hook
  ((before-save . gofmt-before-save)
   (go-mode . memes/go-compile))
  :config
  (validate-setq gofmt-command "goimports")
  :bind
  (:map go-mode-map
        ([remap xref-find-definitions] . godef-jump)
        ("C-c r" . go-remove-unused-imports)
        ("C-c i" . go-goto-imports)))

(use-package lsp-go
  :ensure t
  :defer t
  :commands lsp-go-enable
  :init
  (after (go-mode config-completion config-lsp)
    (memes/completion-add-backends 'go-mode 'company-lsp))
  :hook
  (go-mode . lsp-go-enable))

(use-package flycheck-gometalinter
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
  :config
  (progn
    (flycheck-gometalinter-setup)
    (validate-setq flycheck-gometalinter-vendor t
                   flycheck-gometalinter-test t
                   flycheck-gometalinter-fast t)))

(use-package go-dlv
  :ensure t
  :defer t)

(use-package go-rename
  :ensure t
  :defer t
  :bind
  (:map go-mode-map
        ("C-c r" . go-rename)))

(use-package go-impl
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
  :bind
  (:map go-mode-map
        ("C-c a" . go-test-current-project)
        ("C-c m" . go-test-current-file)
        ("C-c ." . go-test-current-test)
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
