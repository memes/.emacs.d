;;; 99common.el --- Common settings (late)

;;; Commentary:

;;; Code:

(defvar memes-packages)

;; Use yasnippet
(add-to-list 'memes-packages 'yasnippet)
(with-eval-after-load "yasnippet"
  (yas-global-mode 1))

;; Enable company-mode if present
(add-to-list 'memes-packages 'company)
(with-eval-after-load "company"
  (setq-default company-tooltip-align-annotations t
		company-auto-complete t))
(add-hook 'memes-after-load-packages-hook 'global-company-mode)

;; Allow exec-path to inherit from shell
(add-to-list 'memes-packages 'exec-path-from-shell)
(with-eval-after-load "exec-path-from-shell"
  (exec-path-from-shell-initialize))

;; Disable backtrace on error
(setq debug-on-error nil)

(provide '99common)
;;; 99common.el ends here
