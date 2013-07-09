;;; Simple mode for PlantUML files
;;; See plantuml.sourceforge.net
;;; Author: Dominique de Waleffe <ddw@acm.org>
;;; Provides simple functions/bindings to rebuild/resize the images
;;;          a simple mode with key-bndings for those commands
;;;
;;; Started of functions defined at http://gist.github.com/547521
;;;         by Luca Greco
;;; Provides:
;;;     Major mode
;;;     Template
;;;     Local keymap with binding
;;;     Easy customization
;;; Usage:
;;;      A.1) do not forget toset environment variable GRAPHVIZ_DOT
;;;           to full path of dot.exe
;;;      A.2) ensure imageMagick 'convert' is in path of use customize to set
;;;           plu-resize-cmd adequately
;;;      B) in your .emacs
;;;     (autoload 'plantuml-mode "plantuml.el" nil t)
;;;     (push '("\\.uml$" . plantuml-mode) auto-mode-alist)

(provide 'plantuml)
(require 'iimage)

(defgroup PlantUML '((plu-plantuml-jar  custom-variable)
		     (plu-resize-cmd   custom-variable)
		     (plu-force-reload-images custom-variable)
		     (plu-thumbnails-width custom-variable))
  "Group for PlantUML mode configuration")

(defcustom plu-plantuml-jar "plantuml.jar"
  "Where to find the plantuml.jar file"
  :group 'PlantUML :type 'string )

(defcustom plu-resize-cmd "convert"
  "How to call the resize operation.
Defaults to ImageMagick 'convert'.
Use 'gm convert' if using GraphicsMagick"
  :group 'PlantUML :type 'string)

(defcustom plu-thumbnails-width 256
  "Width of thumbnails."
  :group 'PlantUML :type 'integer)

(defcustom plu-force-reload-images t
  "When t, force image reloading on generate"
  :group 'PlantUML :type 'boolean)

;;;
;;; should not be customizable as depends strictly on PlantUML syntax
(defvar plu-image-tag-re
  '("@startuml\s+\\(.+\\)" . 1)
  " Pair with RE matching where to find the file name
and number of the group enclosing the file name part")

;; save history of referenced include files
(defvar plu-incf-history nil)


(define-skeleton plu-schema
  "Skeleton for a new UML schema"
  ""
  "'---FILE:"
  (setq fn (concat (skeleton-read "File:") ".png")) \n
  "@startuml " fn \n
  (let ((inc (read-string "Include file:" nil 'plu-incf-history)))
    (if (> (length inc) 0)
	(concat "!include " inc "\n")
      ""))
  - \n
  "@enduml\n'---\n")


(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist plu-image-tag-re)

(define-derived-mode plantuml-mode text-mode "PlantUML"
  "Major mode for editing PlantUML models.
\\{plantuml-mode-map}"

  (iimage-mode 1)
  (setq comment-start "'")
  (setq imenu-generic-expression plu-imenu-expression)
  (imenu-add-to-menubar "UML")
  (modify-syntax-entry ?' "<")
  (define-key plantuml-mode-map  "\C-c\C-c" 'plu-render-buffer)
  (define-key plantuml-mode-map  "\C-cu" 'plu-render-this)
  (define-key plantuml-mode-map  "\C-cR" 'plu-resize-all-images)
  (define-key plantuml-mode-map  "\C-cr" 'plu-resize-image-at-point)
  (define-key plantuml-mode-map  "\C-cl" 'plu-reload-image-at-point)
  (define-key plantuml-mode-map  "\C-cL" 'plu-reload-all-images)
  (define-key plantuml-mode-map  "\C-ci" 'plu-schema)
  (define-key plantuml-mode-map  "\C-cI" 'iimage-mode)
  (define-key plantuml-mode-map  "\C-cT" 'plu-thumbnail-all-images)
  ;; define in menubar
  (define-key plantuml-mode-map [menu-bar plantuml]
    (cons "PlantUML" (make-sparse-keymap "PlantUML")))
  (define-key plantuml-mode-map [menu-bar plantuml render]
    '("Render" . plu-render-buffer))
  (define-key plantuml-mode-map [menu-bar plantuml render1]
    '("Render this" . plu-render-this))
  (define-key plantuml-mode-map [menu-bar plantuml resize1]
    '("<n>Resize this" . plu-resize-image-at-point))
  (define-key plantuml-mode-map [menu-bar plantuml resize-all]
    '("<n>Resize all" . plu-resize-all-images))
  (define-key plantuml-mode-map [menu-bar plantuml reload-all]
    '("Reload all" . plu-reload-all-images))
  (define-key plantuml-mode-map [menu-bar plantuml toggle-img]
    '("Toggle images" . iimage-mode))
  (define-key plantuml-mode-map [menu-bar plantuml reload-all]
    '("Insert model" . plu-schema))

  (define-key plantuml-mode-map [menu-bar plantuml thumb]
    '("Thumbnail all" . plu-thumbnail-all-images))

  (setq compile-command (plu-make-command buffer-file-name))
  )

(defun plu-make-command (uml-file)
  "Build the command line to run plantuml on given file"
  ;; Note the double quotes are used to keep this simply portable across
  ;; Unix and Windows
  (format "java -jar \"%s\" \"%s\""
	  (expand-file-name plu-plantuml-jar)
	  uml-file))


;; Rendering plantuml
(defun plu-render-buffer (arg)
  "Render all uml blocks in buffer with plantUML. With argument,
automatically resize images to thumbnail width (see plu-thumbnails-width)"
  (interactive "P")
  (let ((resize-to-buf (if arg t nil)))

    (message "PLANTUML Start rendering")
    (shell-command (plu-make-command buffer-file-name)
		   ;; for some reason
		   ;; could get the error output into the buffer
		   ;; if used only one (as should work per doc)
		   (get-buffer-create "*PlantUml*")
		   (get-buffer-create "*PlantUml*"))
    (message (concat "PLANTUML Rendered:  " (buffer-name)))
    ;; force iimage mode
    (iimage-mode 1)
    (if resize-to-buf
      (plu-resize-all-images plu-thumbnails-width)
      ;; force re-display
      (when plu-force-reload-images
	(clear-image-cache)))))

(defun plu-render-this (arg)
  "Render all uml blocks in buffer with plantUML. With argument,
automatically resize images to thumbnail width (see plu-thumbnails-width)"
  (interactive "P")
  (let ((resize-to-buf (if arg t nil))
	;; make tempfile in current-dir
	(tmpfile (let ((temporary-file-directory "."))
		   (make-temp-file "plantUML"))))

    (message (concat "PLANTUML Start rendering " tmpfile))
    ;;TODO
    ;; extract surrounding @startuml @enduml block into tempfile
    (let ((beg (re-search-backward "@startuml"))
	  (end (re-search-forward "@enduml")))
      (if (and beg end)
	  (write-region beg end tmpfile)
	(error "Could not find plantUML block")))
    (shell-command (concat "java -jar " plu-plantuml-jar " "
			   tmpfile)
		   ;; for some reason
		   ;; could not get the error output into the buffer
		   ;; if used only one (as should work per doc)
		   (get-buffer-create "*PlantUml*")
		   (get-buffer-create "*PlantUml*"))
    (delete-file tmpfile)
    (message (concat "PLANTUML Rendered:  " "<current>"))
    ;; force iimage mode
    (iimage-mode 1)
    (if resize-to-buf
      (plu-resize-all-images plu-thumbnails-width)
      ;; force re-display
      (when plu-force-reload-images
	(clear-image-cache)))))



;; Image reloading
(defun plu-reload-image-at-point ()
  (interactive)
  (let* ((image-spec (get-text-property (point) 'display))
	 (file (cadr (member :file image-spec))))
    (message (format "reloading %s at point in the current buffer..." file))
    (image-refresh image-spec)))

(defun plu-reload-all-images()
  (interactive)
  (clear-image-cache))

;; Image resizing and reloading
(defun plu-resize-image-at-point (hsize)
  (interactive "pHorizontal size:")
  (let ((hsize
	 (if (< hsize 100)
	     (* (window-width (selected-window)) (frame-char-width))
	   hsize)))
    ;;(message "resizing image at point in the current buffer123...")
    (let* ((image-spec (get-text-property (point) 'display))
	   (file (cadr (member :file image-spec)))
	   (cmd (format "%s -resize %d \"%s\" \"%s\""
			plu-resize-cmd
			hsize
			file file)))
      (message (format "resizing image: %s %d" file hsize))
      (message cmd)
      (shell-command cmd
		     (get-buffer-create "*PlantUml*")
		     (get-buffer-create "*PlantUml*"))
      (plu-reload-image-at-point))))

(defun plu-resize-all-images(hsize)
  (interactive "pHorizontal size:")
  (let ((hsize
	 (if (< hsize 100)
	     (* (window-width (selected-window)) (frame-char-width))
	   hsize)))
    (let ((re-to-match (car plu-image-tag-re)))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward re-to-match nil t)
	  (forward-char -2)
	  (plu-resize-image-at-point hsize))))))

(defun plu-thumbnail-all-images()
  (interactive)
  (let ((re-to-match (car plu-image-tag-re)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re-to-match nil t)
	(forward-char -2)
	(plu-resize-image-at-point plu-thumbnails-width)))))

;;IMenu support
(defvar plu-imenu-expression
  '( ("Models" "@startuml +\\([^ ]+\\)$" 1)
     ("Definitions" "^!define +\\([^ ]+\\) "  1))
  "values to use for initializing imenu")

;;; TODO: ensure resize keeps the original file
;;; TODO: ensure to get size from user when called from menu-bar
;;; TODO: fontlock mode
