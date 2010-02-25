;; Settings unique to blofeld
;; $Id: 99cookie.el 1 2007-10-18 23:15:33Z memes $
(setq debug-on-error nil)

;; Cookie has Oracle
;;(setq auto-mode-alist (cons '("\\.sql$" . sql-oracle) auto-mode-alist))

;; Fortune integration
(setq fortune-dir "/usr/share/games/fortunes"
      fortune-file (expand-file-name "wisdom" fortune-dir))
(add-hook 'message-setup-hook 'fortune-to-signature)

;; Startup xrefactory if in windowed environment
(if (memq window-system '(x win32 w32))
    (progn
      ;; Xref integration
      (defvar xref-key-binding 'local)
      (setq exec-path (cons "~/xref" exec-path)
          load-path (cons "~/xref/emacs" load-path))
      (load "xrefactory")
      ))
