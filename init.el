;; MEmes emacs setup file, culled from various sources.
;; $Id: dotemacs 7 2008-06-27 15:21:24Z memes $

;; Latest customisations require Emacs24; not supporting anything less than that
(let ((min_supported 24))
  (unless (>= emacs-major-version min_supported)
    (error "Emacs %d or later is required for this configuration" min_supported)))

;; Add MEmes lisp files to load path
(add-to-list 'load-path (convert-standard-filename (expand-file-name "lisp/" user-emacs-directory)))

;; Don't break when loading configuration files
(defun my-load-config (config-file)
  "Load a supplied configuration file, don't fuss if unavailable"
  (if (load config-file t)
    (message (concat "Loaded " config-file))
      (message (concat "Unable to load " config-file ", no matter"))))

;; List of configuration files to be loaded
(defvar memes-config-files
    (list
      "colour-scheme"
      "mutt-config"
      "w3m-config"
      "coding-config"
      "sql-config"
      "markup-config"
      "org-config")
  "List of configuration files to load that can be changed by host config")

;; Load common settings
(my-load-config "00common")

;; Load machine specific settings, if they exist
(my-load-config (concat "01" my-hostname))

;; Load the configuration settings
(dolist (memes-config-file memes-config-files)
  (my-load-config memes-config-file))
  
;; Load common final settings
(my-load-config "98common")

;; Load machine specific final settings, if they exist
(my-load-config (concat "99" my-hostname))
