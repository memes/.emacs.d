;; MEmes emacs setup file, culled from various sources.
;; $Id: dotemacs 7 2008-06-27 15:21:24Z memes $

;; Latest customisations require Emacs24; not supporting anything less than that
(let ((min_supported 24))
  (unless (>= emacs-major-version min_supported)
    (error "Emacs %d or later is required for this configuration" min_supported)))

;; Add MEmes lisp files to load path
(add-to-list 'load-path (convert-standard-filename (expand-file-name "lisp/" user-emacs-directory)))

;; Don't break when loading configuration files
(defun memes-load-config (config-file)
  "Load a supplied configuration file, don't fuss if unavailable"
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
  "List of configuration files to load that can be changed by host config")

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
