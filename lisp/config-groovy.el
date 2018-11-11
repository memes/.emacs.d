;;; config-groovy.el --- Configure Groovy and Grails support

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package groovy-mode
  :ensure t
  :defer t
  :hook
  (groovy-mode . groovy-electric-mode))

(provide 'config-groovy)
;;; config-groovy.el ends here
