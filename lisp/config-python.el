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
  (inferior-python-mode-hook . (lambda ()
                                 (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
                                 (process-query-on-exit-flag (get-process "Python")))))

(use-package py-yapf
  :ensure t
  :defer t
  :hook
  (python-mode . py-yapf-enable-on-save))

(use-package lsp-python
  :ensure t
  :defer t
  :commands lsp-python-enable
  :init
  (after (config-completion config-lsp)
    (memes/completion-add-backends 'python-mode 'company-lsp))
  :hook
  (python-mode . lsp-python-enable))

(use-package dap-python
  :after (config-lsp lsp-python))

(provide 'config-python)
;;; config-python.el ends here
