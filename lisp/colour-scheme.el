;; Define a custom colour scheme
;; $Id: colour-scheme.el 1 2007-10-18 23:15:33Z memes $

;; Solarized theme requires dash
(add-to-list 'memes-package-archives '("melpa" . "http://stable.melpa.org/packages/"))
(add-to-list 'memes-packages 'solarized-theme)

(defun memes-set-theme ()
    (load-theme 'solarized-dark t))
(add-hook 'emacs-startup-hook 'memes-set-theme)


;; Colour syntax highlighting
(cond ((fboundp 'global-font-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)
       ;; Maximum colours
       (setq font-lock-maximum-decoration t))
      )
