;; Settings common to all hosts to be executed last
;; $Id: 98common.el 1 2007-10-18 23:15:33Z memes $

;; Use yasnippet
(add-to-list 'memes-packages 'yasnippet)
(with-eval-after-load "yasnippet"
  (yas-global-mode 1))

;; Enable auto-complete if present
;; Autocomplete package
(add-to-list 'memes-packages 'auto-complete)
(with-eval-after-load "auto-complete"
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

;; Allow exec-path to inherit from shell
(add-to-list 'memes-packages 'exec-path-from-shell)
(with-eval-after-load "exec-path-from-shell"
  (exec-path-from-shell-initialize))

;; Disable backtrace on error
(setq debug-on-error nil)
