;; W3M configuration
;; $Id: w3m-config.el 7 2008-06-27 15:21:24Z memes $

(autoload 'w3m "w3m" t);
(eval-after-load "w3m"
  '(progn
     (setq browse-url-browser-function 'w3m-browse-url
	   w3-default-homepage "http://www.google.com/"
	   w3-do-incremental-display t
	   w3-delay-image-loads t
	   w3-right-margin 1
	   w3-maximum-line-length nil
	   browse-url-netscape-program "/usr/bin/www-browser")
     ))