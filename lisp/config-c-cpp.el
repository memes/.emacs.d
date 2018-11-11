;;; config-c-cpp.el --- Configure C/C++ mode for Emacs

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defcustom memes/c-basic-offset 4
  "The default indentation size to use for derived cc-modes."
  :type 'integer
  :group 'memes/c-cpp)

(defcustom memes/c-cpp-mode-for-headers 'c-mode
  "Which mode to use for header files; can be `c-mode` or `c++-mode`."
  :type 'symbol
  :options '(c-mode c++-mode)
  :group 'memes/c-cpp)

(use-package cc-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist
                 `("\\.h\\'" . memes/c-cpp-mode-for-headers)))
  :config
  (progn
    (require 'compile)
    (setq-default c-basic-offset memes/c-basic-offset
                  tab-width memes/c-basic-offset
                  indent-tabs-mode nil)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'statement-case-open 0)
    (c-set-offset 'case-label '+)
    (c-toggle-auto-newline 1)))

(use-package clang-format
  :ensure t
  :defer t
  :commands (clang-format-region))

;; Helper functions to enable ccls and LSP in a specific mode

;;;###autoload
(defvar memes/ccls-path-mappings [])

;;;###autoload
(defun memes/ccls-enable ()
  "Enable lsp/ccls integration."
  (setq-local lsp-ui-sideline-show-symbol nil)
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))

;; Use ccls as c/c++/obj-c server for LSP
(use-package ccls
  :ensure t
  :defer t
  :preface
  (progn
    ;; recommended ccls handlers - from MaskRay's wiki
    ;; https://github.com/MaskRay/ccls/wiki/Emacs
    (defun ccls/callee ()
      (interactive)
      (lsp-ui-peek-find-custom 'callee "$ccls/call" '(:callee t)))

    (defun ccls/caller ()
      (interactive)
      (lsp-ui-peek-find-custom 'caller "$ccls/call"))

    (defun ccls/vars (kind)
      (lsp-ui-peek-find-custom 'vars "$ccls/vars" `(:kind ,kind)))

    (defun ccls/base (levels)
      (lsp-ui-peek-find-custom 'base "$ccls/inheritance" `(:levels ,levels)))

    (defun ccls/derived (levels)
      (lsp-ui-peek-find-custom 'derived "$ccls/inheritance" `(:levels ,levels :derived t)))

    (defun ccls/member ()
      (interactive)
      (lsp-ui-peek-find-custom 'member "$ccls/member")))
  :commands (lsp-ccls-enable)
  :config
  (progn
    (validate-setq ccls-cache-dir ".ccls-cache"
                   ccls-sem-highlight-method 'font-lock
                   ccls-extra-init-params `(:clang (:pathMappings ,memes/ccls-path-mappings)
                                                   :completion
                                                   (
                                                    :includeBlacklist ("^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
                                                                       "^/usr/(local/)?include/c\\+\\+/v1/"))
                                                   :diagnostics (:frequencyMs 5000)
                                                   :index (:reparseForDependency 1)))
    (ccls-use-default-rainbow-sem-highlight)
    (memes/completion-add-backends 'c-mode 'company-lsp)
    (memes/completion-add-backends 'c++-mode 'company-lsp)
    (memes/completion-add-backends 'objc-mode 'company-lsp)
    (after 'projectile
      (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")))
  :hook
  ((c-mode . memes/ccls-enable)
   (c++-mode . memes/ccls-enable)
   (objc-mode . memes/ccls-enable)))

(provide 'config-c-cpp)
;;; config-c-cpp.el ends here
