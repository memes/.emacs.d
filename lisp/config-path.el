;;; config-path.el --- Emacs path configuration

;;

;;; Commentary:

;;

;;; Code:

;;;###autoload
(defun memes/file (name)
  "Return the file or directory matching `NAME` from `user-emacs-directory`."
  (expand-file-name name user-emacs-directory))

;; Put my customisations in a file that is excluded from git!
(setq custom-file (memes/file "custom.el"))

;; Location of any local lisp files.
(defcustom memes/lisp-dir
  (expand-file-name "lisp" user-emacs-directory)
  "Directory that contains local Lisp files."
  :type 'directory
  :group 'memes/path)

;; Get a full qualified file from my lisp directory.
;;;###autoload
(defun memes/lisp (name)
  "Return a file with `NAME` from my Lisp directory."
  (expand-file-name name memes/lisp-dir))

;; Location of any local etc files.
(defcustom memes/etc-dir
  (expand-file-name "etc" user-emacs-directory)
  "Directory that contains local etc files."
  :type 'directory
  :group 'memes/path)

;; Get a full qualified file from my etc directory.
;;;###autoload
(defun memes/etc (name)
  "Return a file with `NAME` from my etc directory."
  (expand-file-name name memes/etc-dir))

;; Location of any local var files.
(defcustom memes/var-dir
  (expand-file-name "var" user-emacs-directory)
  "Directory that contains local var files."
  :type 'directory
  :group 'memes/path)

;; Get a full qualified file from my var directory.
;;;###autoload
(defun memes/var (name)
  "Return a file with `NAME` from my var directory."
  (expand-file-name name memes/var-dir))

;; Location of any local lib files.
(defcustom memes/lib-dir
  (expand-file-name "lib" user-emacs-directory)
  "Directory that contains local lib files."
  :type 'directory
  :group 'memes/path)

;; Get a full qualified file from my lib directory.
;;;###autoload
(defun memes/lib (name)
  "Return a file with `NAME` from my lib directory."
  (expand-file-name name memes/lib-dir))

;; Other directories to add to load-path.
(defcustom memes/other-load-path-dirs
  ()
  "List of additional directories to add to `load-path`."
  :type 'directory
  :group 'memes/path)

;; Function to add a directory, and subdirectories, to load-path.
;;;###autoload
(defun memes/add-load-path (dir)
  "Add `DIR` and any subdirectories to `load-path`."
  (let ((default-directory dir))
    (normal-top-level-add-to-load-path (list dir))
    (normal-top-level-add-subdirs-to-load-path)
    (delete-dups load-path)))

;; Add my lisp dir to load-path.
;;;###autoload
(defun memes/add-load-paths ()
  "Add Lisp directory to `load-path`."
  (memes/add-load-path memes/lisp-dir)
  (dolist (dir memes/other-load-path-dirs)
    (memes/add-load-path dir)))

;; Add all extra dirs to load-path.
;;;###autoload
(memes/add-load-paths)


(provide 'config-path)
;;; config-path.el ends here
