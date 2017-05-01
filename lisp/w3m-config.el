;;; w3m-config.el --- integration with w3m

;;; Commentary:

;;; Code:

(defvar memes-packages)

(add-to-list 'memes-packages 'w3m)
(with-eval-after-load "w3m"
  '(progn
     (setq browse-url-browser-function 'w3m-browse-url
	   w3-default-homepage "http://www.google.com/"
	   w3-do-incremental-display t
	   w3-delay-image-loads t
	   w3-right-margin 1
	   w3-maximum-line-length nil
	   browse-url-netscape-program "/usr/bin/www-browser")
     ))

(provide 'w3m-config)
;;; w3m-config.el ends here
