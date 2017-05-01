;;; colour-scheme.el --- colour scheme for Emacs

;;; Commentary:

;;; Code:

(defvar memes-packages)

(add-to-list 'memes-packages 'solarized-theme)

;; Register a hook to load a theme and turn on font decorations
;;  - do in startup hook so that the theme can be downloaded and installed from
;;    MELPA if necessary
(defun memes-set-theme ()
  "Load my current theme."
  (load-theme 'solarized-dark t)
  (cond ((fboundp 'global-font-lock-mode)
	 ;; Turn on font-lock in all modes that support it
	 (global-font-lock-mode t)
	 ;; Maximum colours
	 (setq font-lock-maximum-decoration t))
	))
(add-hook 'memes-after-load-packages-hook 'memes-set-theme)

(provide 'colour-scheme)
;;; colour-scheme.el ends here
