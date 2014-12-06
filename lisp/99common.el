;; Settings common to all hosts to be executed last
;; $Id: 98common.el 1 2007-10-18 23:15:33Z memes $

;; Enable auto-complete if present
;; Autocomplete package
(add-to-list 'memes-packages 'auto-complete)
(with-eval-after-load "auto-complete"
  (global-auto-complete-mode t))

;; Disable backtrace on error
(setq debug-on-error nil)
