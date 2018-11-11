;;; config-platform.el --- Make any OS/platform specific changes needed

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defconst memes/is-linux (eq system-type 'gnu/linux))
(defconst memes/is-mac (eq system-type 'darwin))
(defconst memes/is-windows (eq system-type 'windows-nt))

(defvar memes/local-libs-root
  (expand-file-name
   (cond (memes/is-mac "~/Library")
	 (t "~/lib")))
  "Location of my locally installed libraries, tools and extensions.")


(provide 'config-platform)
;;; config-platform.el ends here
