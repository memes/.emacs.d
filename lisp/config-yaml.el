;;; config-yaml.el --- Support working with YAML files

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.\\(yaml\\|yml\\)\\'" . yaml-mode)
         ("\\.raml\\'" . yaml-mode))
  :init
  (after 'config-markdown
    (memes/add-markdown-lang-mode "yaml" 'yaml-mode)))

(provide 'config-yaml)
;;; config-yaml.el ends here
