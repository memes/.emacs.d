;;; config-ansible.el --- Support working with ansible configurations

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package ansible
  :ensure t
  :defer t
  :after yaml-mode
  :mode (("group_vars/.*" . yaml-mode)
         ("host_vars/.*" . yaml-mode))
  :init
  (after 'config-markdown
    (memes/add-markdown-lang-mode "ansible" 'yaml-mode))
  :config
  (progn
    (defun memes/ansible-is-prog-mode ()
      "Treat ansible files as extension of prog-mode."
      (run-hooks 'prog-mode-hook))
    (defvar memes/vault-pass-filename ".vault_pass"
      "The filename for vault passwords.")
    (defun memes/ansible-locate-vault-pass-file ()
      "Find the vault password file from current directory."
      (when-let ((dir (locate-dominating-file "." memes/vault-pass-filename)))
        (expand-file-name memes/vault-pass-filename dir)))
    (defun memes/set-ansible-vault-pass ()
      "Set the locations of the vault password file."
      (validate-setq ansible::vault-password-file (memes/ansible-locate-vault-pass-file))))
  :hook
  ((yaml-mode . ansible)
   (ansible . memes/ansible-is-prog-mode)
   (ansible . memes/set-ansible-vault-pass)
   (ansible . ansible::auto-decrypt-encrypt)))

(use-package jinja2-mode
  :defer t
  :ensure t
  :mode "\\.j2?\\'")

(use-package ansible-doc
  :ensure t
  :defer t
  :after ansible
  :hook
  (ansible::hook . ansible-doc-mode))

(use-package company-ansible
  :ensure t
  :defer t
  :after ansible
  :init
  (after config-completion
    (memes/completion-add-backends 'yaml-mode (memes/completion-backend-with-yasnippet #'company-ansible))))

(provide 'config-ansible)
;;; config-ansible.el ends here
