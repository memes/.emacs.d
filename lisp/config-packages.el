;;; config-packages.el --- Emacs package initialisation

;;

;;; Commentary:

;;

;;; Code:
(require 'package)
(require 'subr-x)
(require 'tls)
(require 'gnutls)

;; Package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))

;; Mozilla CA certificate bundle for TLS verification
(defcustom memes/ca-bundle
  (memes/etc "cacert.pem")
  "Path to trusted CA certificate bundle."
  :type 'file
  :group 'memes/packages)
  
;; TLS options for HTTPS
(setq gnutls-verify-error t
      tls-checktrust t
      tls-program `(,(format "gnutls-cli --x509cafile %s -p %%p %%h" memes/ca-bundle)))

(defun memes/config-test-tls ()
  "Error if TLS isn't properly configured."
  (let ((bad-hosts
         (cl-loop for bad
                  in `("https://wrong.host.badssl.com/"
                       "https://self-signed.badssl.com/")
                  if (condition-case _e
                         (url-retrieve
                          bad (lambda (_retrieved) t))
                       (error nil))
                  collect bad)))
    (if bad-hosts
        (error (format "tls misconfigured; retrieved %s ok" bad-hosts)) ;
      (url-retrieve "https://badssl.com" (lambda (_retrieved) t)))))

;; Setup package.el and use-package.
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; ** paradox

(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(provide 'config-packages)
;;; config-packages.el ends here
