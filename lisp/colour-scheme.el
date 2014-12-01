;; Define a custom colour scheme
;; $Id: colour-scheme.el 1 2007-10-18 23:15:33Z memes $

;; Solarized theme requires dash
(add-to-list 'memes-package-archives '("melpa" . "http://stable.melpa.org/packages/"))
(add-to-list 'memes-packages 'solarized-theme)

(defvar memes-preferred-theme 'solarized-dark
  "Preferred theme to use")

;; Register a hook to load a theme and turn on font decorations
;;  - do in startup hook so that the theme can be downloaded and installed from
;;    MELPA if necessary
(defun memes-set-theme ()
  (load-theme memes-preferred-theme t)
  (cond ((fboundp 'global-font-lock-mode)
	 ;; Turn on font-lock in all modes that support it
	 (global-font-lock-mode t)
	 ;; Maximum colours
	 (setq font-lock-maximum-decoration t))
	))
(add-hook 'emacs-startup-hook 'memes-set-theme)