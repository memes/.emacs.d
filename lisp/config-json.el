;;; config-json.el --- Configure JSON support

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defconst memes/json-indent 2
  "Default indentation for json.")

(use-package json-mode
  :ensure t
  :defer t
  :after (config-javascript)
  :config
  (validate-setq json-reformat:indent-width memes/json-indent)
  :mode "\\.json\\'")

(provide 'config-json)
;;; config-json.el ends here
