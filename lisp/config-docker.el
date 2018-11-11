;;; config-docker.el --- Configuration for docker support

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package dockerfile-mode
  :ensure t
  :defer t
  :after (config-yaml))

(use-package docker
  :ensure t
  :defer t
  :bind
  ("C-c d" . docker))

(provide 'config-docker)
;;; config-docker.el ends here
