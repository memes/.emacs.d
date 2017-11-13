;;; irc-config.el --- Configuration for IRC (and Slack)

;;; Commentary:

;;; Code:

(defvar memes-packages)

;; Emojis - why not
(add-to-list 'memes-packages 'emojify)
(add-hook 'memes-after-load-packages-hook #'global-emojify-mode)

(add-to-list 'memes-packages 'company-emoji)
(with-eval-after-load 'company-emoji
  (add-to-list 'company-backends 'company-emoji))

(defun memes-fetch-password (&rest params)
  "Lookup an auth-source password."
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
	(let ((secret (plist-get match :secret)))
	  (if (functionp secret)
	      (funcall secret)
	    secret))
      (error "Password not found for %S" params))))

(defun memes-freenode-password (server)
  "Return the password for memes on freenode."
  (memes-fetch-password :user "matthewemes" :host "irc.freenode.net"))
  
;; circe
(add-to-list 'memes-packages 'circe)
(setq circe-use-cycle-completion t
      circe-reduce-lurker-spam t
      circe-network-options '(("Freenode"
			       ;; TLS is failing
			       :tls nil
			       :nick "memes"
			       :user "matthewemes"
			       :sasl-username "matthewemes"
			       :sasl-password memes-freenode-password
			       :nickserv-password memes-freenode-password
			       :channels ("#emacs" "#debian" "#chromium-os"
					  "#gcloud" "#appengine" "#googleglass"
					  "#termux")))
      lui-time-stamp-position 'left-margin
      lui-time-stamp-format "%H:%M"
      lui-flyspell-p t
      lui-flyspell-alist '((".*" "en_GB")))
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(defun memes-circe-network-connected-p (network)
  "Return non-nil if there is an existing circe buffer whose `circe-network` is NETWORK."
  (catch 'return
    (dolist (buffer-base-buffer (circe-server-buffers))
      (with-current-buffer buffer
	(if (string= network circe-network)
	    (throw 'return t))))))

(defun memes-circe-maybe-connect (network)
  "Connect to NETWORK, but ask for confirmation if it's already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (memes-circe-network-connected-p network))
	  (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(defun memes-circe-set-margin ()
  "Sets up the margins for circe."
  (setq left-margin-width 5))
(add-hook 'lui-mode-hook 'memes-circe-set-margin)

(defun memes-circe-mode-hook ()
  "Hook for circe."
  (circe-set-display-handler "JOIN" (lambda (&rest ignored) nil)))
(add-hook 'circe-mode-hook 'memes-circe-mode-hook)

(defun memes-start-irc ()
  "Start IRC."
  (interactive)
  (require 'circe)
  (memes-circe-maybe-connect "Freenode"))

(global-set-key (kbd "C-c i") 'memes-start-irc)

(add-to-list 'memes-packages 'slack)
(with-eval-after-load 'slack
  (setq slack-buffer-emojify t
	slack-prefer-current-team t))
(add-to-list 'memes-packages 'alert)
(with-eval-after-load 'alert
  (setq alert-default-style 'notifier))

(provide 'irc-config)
;;; irc-config.el ends here
