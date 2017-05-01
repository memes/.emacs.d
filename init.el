;;; init.el --- initialise Emacs

;;; Commentary:

;;; Code:

(defvar memes-hostname
  (substring (system-name) 0
	     (string-match "\\..+" (system-name)))
  "Contains the name of this system; used for buffer identification, etc.
Defaults to short hostname.")

;; Latest customisations require Emacs24; not supporting anything less than that
(let ((min_supported 24))
  (unless (>= emacs-major-version min_supported)
    (error "Emacs %d or later is required for this configuration" min_supported)))

(defvar memes-lisp
  (locate-user-emacs-file "lisp")
  "Path to local elisp files and configurations.")

;; (Re-)compile lisp files as necessary
(byte-recompile-directory memes-lisp 0)

;; Add MEmes lisp files to load path
(add-to-list 'load-path memes-lisp)

;; Don't break when loading configuration files
(defun memes-load-config (config-file)
  "Load supplied CONFIG-FILE without throwing an error if it is unavailable."
  (if (load config-file t)
      (message (concat "Loaded " config-file))
    (message (concat "Unable to load " config-file ", ignoring"))))

;; List of configuration files to be loaded
(defvar memes-config-files
  (list
   "colour-scheme"
   "mutt-config"
   "w3m-config"
   "org-config"
   "coding-config"
   "sql-config"
   "markup-config"
   )
  "List of configuration files to load that can be changed by host config.")

;; Load common settings
(memes-load-config "00common")

;; Load machine specific settings, if they exist
(memes-load-config (concat "01" memes-hostname))

;; Load the configuration settings
(dolist (memes-config-file memes-config-files)
  (memes-load-config memes-config-file))

;; Load machine specific final settings, if they exist
(memes-load-config (concat "98" memes-hostname))
  
;; Load common final settings
(memes-load-config "99common")

(provide 'init)
;;; init.el ends here
