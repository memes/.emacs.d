;;; config-python.el --- Configure python support on Emacs

;;

;;; Commentary:

;; $ global_pip2 install yapf python-language-server
;; $ global_pip3 install yapf python-language-server

;;; Code:
(require 'use-config)

(defcustom memes/python-basic-offset 4
  "The default indentation size to use for python."
  :type 'integer
  :group 'memes/python)

(use-package python
  :ensure nil
  :defines gud-pdb-command-name pdb-path
  :config
  (progn
    (validate-setq python-shell-completion-native-enable nil)
    (setq pdb-path 'pdb
          gud-pdb-command-name (symbol-name pdb-path))
    (defadvice pdb (before gud-query-cmdline activate)
      "Provide a better default command line when called interactively."
      (interactive
       (list (gud-query-cmdline
              pdb-path
              (file-name-nondirectory buffer-file-name))))))
  :hook
  ((python-mode . lsp)
   (inferior-python-mode-hook . (lambda ()
                                  (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
                                  (process-query-on-exit-flag (get-process "Python"))))))

(use-package py-yapf
  :ensure t
  :defer t
  :hook
  (python-mode . py-yapf-enable-on-save))

(use-package dap-python
  :after config-lsp)

(use-package pipenv
  :ensure t
  :defer t
  :after projectile
  :init
  (setq-default pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  :hook (python-mode . pipenv-mode))

(provide 'config-python)
;;; config-python.el ends here
