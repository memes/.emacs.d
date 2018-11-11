;;; config-terraform.el --- Support working with Terraform files

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package terraform-mode
  :ensure t
  :defer t
  :init
  (after 'config-markdown
    (memes/add-markdown-lang-mode "hcl" 'terraform-mode))
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(use-package company-terraform
  :ensure t
  :defer t
  :init
  (after 'terraform-mode
    (memes/completion-add-backends 'terraform-mode 'company-terraform))
  :config
  (setq-local company-tooltip-limit 3))

(provide 'config-terraform)
;;; config-terraform.el ends here
