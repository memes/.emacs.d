;;; config-irc.el --- Configuration for IRC

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)
(require 'auth-source)

(defun memes/fetch-password (&rest params)
  "Lookup an auth-source password."
  (let ((match (car (apply 'auth-source-search params))))
    (if match
	(let ((secret (plist-get match :secret)))
	  (if (functionp secret)
	      (funcall secret)
	    secret))
      (error "Password not found for %S" params))))

(defun memes/freenode-password (server)
  "Return the password for memes on freenode."
  (memes/fetch-password :user "matthewemes" :host "irc.freenode.net"))

(defun memes/circe-network-connected-p (network)
  "Return non-nil if there is an existing circe buffer whose `circe-network` is NETWORK."
  (catch 'return
    (dolist (buffer-base-buffer (circe-server-buffers))
      (with-current-buffer buffer
	(if (string= network circe-network)
	    (throw 'return t))))))

(defun memes/circe-maybe-connect (network)
  "Connect to NETWORK, but ask for confirmation if it's already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (memes/circe-network-connected-p network))
	  (y-or-n-p (format "Already connected to %s, reconnect? " network)))
      (circe network)))

(defun memes/circe-set-margin ()
  "Sets up the margins for circe."
  (setq left-margin-width 5))

(defun memes/circe-mode-hook ()
  "Hook for circe."
  (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil)))

(defun memes/start-irc ()
  "Start IRC."
  (interactive)
  (require 'circe)
  (memes/circe-maybe-connect "Freenode"))

(use-package circe
  :ensure t
  :defer t
  :config
  (progn
    (setq circe-use-cycle-completion t
          circe-reduce-lurker-spam t
          circe-network-options '(("Freenode"
			           :tls t
			           :nick "memes"
			           :user "matthewemes"
			           :sasl-username "matthewemes"
			           :sasl-password memes/freenode-password
			           :nickserv-password memes/freenode-password
			           :channels ("#emacs"
                                              "#debian"
                                              "#emacs-circe"
					      "#termux")))
          lui-time-stamp-position 'left-margin
          lui-time-stamp-format "%H:%M"
          lui-flyspell-p t
          lui-flyspell-alist '(("." "british")))
    (add-hook 'lui-mode-hook #'memes/circe-set-margin)
    (add-hook 'circe-channel-mode-hook #'enable-lui-autopaste)
    (add-hook 'circe-mode-hook #'memes/circe-mode-hook)))

(provide 'config-irc)
;;; config-irc.el ends here
