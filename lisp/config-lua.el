;;; config-lua.el --- configures lua in Emacs

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defcustom memes/lua-indent-level 2
  "The default indentation level to use with lua."
  :type 'integer
  :group 'memes/lua)

(defcustom memes/lua-indent-string-contents t
  "Set to true to set indentation offset of strings."
  :type 'boolean
  :group 'memes/lua)

(use-package lua-mode
  :ensure t
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :config
  (progn
    (validate-setq lua-indent-level memes/lua-indent-level
                   lua-indent-string-contents memes/lua-indent-string-contents)))

(provide 'config-lua)
;;; config-lua.el ends here
