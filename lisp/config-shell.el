;;; config-shell.el --- Configure shell support in Emacs

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package comint
  :defer t
  :config
  (validate-setq comint-prompt-read-only t))

;; Set executable bit after saving a shell script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package eshell
  :defer t
  :config
  (after 'counsel
    (add-hook 'eshell-mode-hook (lambda ()
                                  (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)))))

(use-package esh-mode
  :defer t
  :functions (eshell/clear eshell/x eshell/clear!)
  :commands (eshell-send-input)
  :preface
  (progn
    (unless (boundp 'eshell/clear)
      (defun eshell/clear (_)
        (let ((inhibit-read-only t))
          (erase-buffer))))
    (defun eshell/x ()
      (kill-buffer)
      (delete-window))
    (defun eshell/clear! ()
      (interactive)
      (eshell/clear t)
      (eshell-send-input)))
  :config
  (progn
    (validate-setq eshell-scroll-to-bottom-on-output 'this
                   eshell-scroll-show-maximum-output t)
    (add-hook 'eshell-mode-hook (lambda ()
                                  (bind-key "C-l" #'eshell/clear! eshell-mode-map)))))

(use-package em-unix
  :defer t
  :config
  (validate-setq eshell-cp-interactive-query t
                 eshell-ln-interactive-query t
                 eshell-mv-interactive-query t
                 eshell-rm-interactive-query t
                 eshell-mv-overwrite-files nil))

(use-package em-cmpl
  :defer t
  :config
  (validate-setq eshell-cmpl-ignore-case t))

(use-package em-term
  :defer t
  :config
  (validate-setq eshell-destroy-buffer-when-process-dies t
                 eshell-visual-commands (append '("tmux" "screen" "ssh" "htop" "git log") eshell-visual-commands)))

(use-package em-hist
  :defer t
  :config
  (validate-setq eshell-hist-ignoredups t))

(use-package eshell-prompt-extras
  :ensure t
  :defer t
  :after em-prompt
  :commands epe-theme-lambda
  :init
  (after 'em-prompt
    (validate-setq eshell-highlight-prompt t
                   eshell-prompt-function #'epe-theme-lambda)))


(provide 'config-shell)
;;; config-shell.el ends here
