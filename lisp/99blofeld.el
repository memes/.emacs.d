;; Settings unique to blofeld
;; $Id: 99blofeld.el 1 2007-10-18 23:15:33Z memes $
(setq debug-on-error nil)

;; Blofeld has Oracle
;;(setq auto-mode-alist (cons '("\\.sql$" . sql-oracle) auto-mode-alist))

;; Fortune integration
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file (expand-file-name "wisdom" fortune-dir))
(add-hook 'message-setup-hook 'fortune-to-signature)
