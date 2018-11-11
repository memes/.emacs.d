;;; config-makefile.el --- Configuration for makefile mode

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package makefile
  :defer t
  :preface
  (defun memes/makefile-hook ()
    "Hook to set indentation values for makefiles."
    (set (make-local-variable 'indent-tabs-mode) t)
    (set (make-local-variable 'tab-width) 4))
  :config
  (add-hook 'makefile-mode-hook #'memes/makefile-hook))

(provide 'config-makefile)
;;; config-makefile.el ends here
