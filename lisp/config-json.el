;;; config-json.el --- Configure JSON support

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package json-mode
  :ensure t
  :defer t
  :after (config-javascript)
  :mode "\\.json\\'")

(provide 'config-json)
;;; config-json.el ends here
