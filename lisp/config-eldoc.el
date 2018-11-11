;;; config-eldoc.el --- Configure eldoc and other helpers

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package eldoc
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'eldoc-mode)
  :config
  (progn
    (validate-setq eldoc-idle-delay 0.5)
    (after 'paredit (eldoc-add-command 'paredit-backward
				       'paredit-forward
				       'paredit-backward-delete
				       'paredit-close-round))))

(use-package know-your-http-well
  :ensure t
  :defer t)

(use-package google-this
  :ensure t)

(use-package niceify-info
  :ensure t
  :after info)

(provide 'config-eldoc)
;;; config-eldoc.el ends here
