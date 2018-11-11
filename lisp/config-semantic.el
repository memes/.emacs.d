;;; config-semantic.el --- Emacs semantic mode configuration

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(defvar memes/semanticdb-dir (memes/var "semanticdb/")
  "The default location for semanticdb files.")

(use-package semantic
  :defer t
  :init (after-init #'semantic-mode)
  :functions
  (semanticdb-file-table-object
   semanticdb-save-all-db)
  :config
  (progn
    (defun memes/semantic-parse-recursively (file-or-dir)
      "Recursively parse all files `FILE-OR-DIR`."
      (cond
       ((null file-or-dir) nil)
       ((file-directory-p file-or-dir)
        (mapcar #'memes/semantic-parse-recursively
                (directory-files-recursively file-or-dir ".+\\.el\\(\\.gz\\)?$")))
       (t (ignore-errors
            (semanticdb-file-table-or-object file-or-dir)))))
    (defun memes/semantic-parse-load-path()
      "Parse all elisp files in 'load-path'."
      (interactive)
      (dolist (path load-path) (memes/semantic-parse-recursively path))
      (semanticdb-save-all-db))))

(use-package semantic/db
  :defer t
  :defines
  (semanticdb-database-list
   semanticdb-file-table-object)
  :config
  ;; Redefined as a fix for:
  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=22287
  (defun semanticdb-save-all-db-idle ()
    "Save all semantic tag databases from idle time.
Exit the save between databases if there is user input."
    (save-excursion
      (semantic-exit-on-input 'semantic-idle-save
        (mapc (lambda (db)
                (semantic-throw-on-input 'semanticdb-idle-save)
                (semanticdb-save-db db t))
              semanticdb-database-list)))))

(use-package semantic/db-file
  :defer t
  :init
  (progn
    (make-directory memes/semanticdb-dir :parents)
    (setq semanticdb-default-save-directory memes/semanticdb-dir)))

(provide 'config-semantic)
;;; config-semantic.el ends here
