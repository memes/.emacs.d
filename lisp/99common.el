;; Settings common to all hosts to be executed last
;; $Id: 98common.el 1 2007-10-18 23:15:33Z memes $

;; Use yasnippet
(add-to-list 'memes-packages 'yasnippet)
(with-eval-after-load "yasnippet"
  (yas-global-mode 1))

;; Enable company-mode if present
(add-to-list 'memes-packages 'company)
(with-eval-after-load "company"
  (setq company-tooltip-align-annotations t
	company-auto-complete t))
(add-hook 'memes-after-load-packages-hook 'global-company-mode)

;; Allow exec-path to inherit from shell
(add-to-list 'memes-packages 'exec-path-from-shell)
(with-eval-after-load "exec-path-from-shell"
  (exec-path-from-shell-initialize))

;; Disable backtrace on error
(setq debug-on-error nil)
