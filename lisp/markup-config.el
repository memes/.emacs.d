;; Options used when editing various markup languages
;; $Id: markup-config.el 5 2007-10-20 23:05:29Z memes $

;; Configure Flyspell for xml/nxml modes
(put 'xml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
(put 'nxml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)

;; PHP support - disabled, use the one that comes with nXhtml mode
;;(require 'php-mode)
;;(autoload 'php-mode "php-mode" "Major mode to edit PHP files." t)

;; SGML/XML handling via psgml/nxml-mode
(require 'rng-loc)
(autoload 'nxml-mode  "nxml-mode" "Major mode to edit XML files." t)
(setq auto-mode-alist
      (append '(("\\.\\(xml\\|xsd\\|db\\|xhtml\\)$" . nxml-mode)
		("\\.\\(xsl\\|fo\\|rng\\|rng\\)$" . nxml-mode)
		) auto-mode-alist))
;; nXML configuration
(setq rng-schema-locating-files (append 
				 '(convert-standard-filename (expand-file-name "schemas/schemas.xml" user-emacs-directory))
				 rng-schema-locating-files
				 )
      nxml-slash-auto-complete-flag t
      nxml-sexp-element-flag t)

;; Load Norman Walsh's unicode support libraries
(require 'cl)
(setq unicode-character-list-file (convert-standard-filename (expand-file-name "lisp/unichars.el" user-emacs-directory)))
(load "xmlunicode")

;; Stolen from "drkm" post on nxml list
(defun surround-region-with-tag (tag-name beg end)
  (interactive "sTag name: \nr")
  (save-excursion
    (goto-char beg)
    (insert "<" tag-name ">")
    (goto-char (+ end 2 (length tag-name)))
    (insert "</" tag-name ">")))

;; Define the nxml entry config
(defun my-nxml-mode-hook ()
  "Prepare nXML mode"
  (auto-fill-mode t)
  (set-fill-column 78)
  (flyspell-mode)
  (set-language-environment "utf-8")
  (define-key nxml-mode-map "\C-r" 'surround-region-with-tag)
  (define-key nxml-mode-map [\C-tab] 'nxml-complete)
  (define-key nxml-mode-map "\"" 'unicode-smart-double-quote)
  (define-key nxml-mode-map "\'" 'unicode-smart-single-quote)
  (define-key nxml-mode-map "\-" 'unicode-smart-hyphen)
  (define-key nxml-mode-map "\." 'unicode-smart-period)
  (define-key nxml-mode-map [menu-bar unichar]
	      (cons "UniChar" unicode-character-menu-map))
  (set-input-method 'xml))
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
;; DTD mode
(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd" 
  "Execute etags on FILESPEC and match on DTD-specific regular expressions." t)
(autoload 'dtd-grep "tdtd" 
  "Grep for PATTERN in files matching FILESPEC." t)
(add-hook 'dtd-mode-hooks	'turn-on-font-lock)
(setq auto-mode-alist 
      (append (list '("\\.dcl$" . dtd-mode)
		    '("\\.dec$" . dtd-mode)
		    '("\\.dtd$" . dtd-mode)
		    '("\\.ele$" . dtd-mode)
		    '("\\.ent$" . dtd-mode)
		    '("\\.mod$" . dtd-mode))
	      auto-mode-alist))

;; CSS mode - show hex values as colours
(require 'cl)
(defun hexcolour-luminance (colour)
  "Calculate the luminance of a colour string (e.g. \"#ffaa00\", \"blue\").
This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values colour))
	 (r (car values))
	 (g (cadr values))
	 (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))
(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil
			  `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
				      (regexp-opt (x-defined-colors) 'words))
			    (0 (let ((colour (match-string-no-properties 0)))
				 (put-text-property
				  (match-beginning 0)
				  (match-end 0)
				  'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
							    "white" "black"))
					  (:background ,colour)))))))))
(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'html-helper-mode-hook 'hexcolour-add-to-font-lock)
(setq html-helper-htmldtd-version
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n  \"http://www.w3.org/TR/html4/strict.dtd\">\n")

;; VTL mode for velocity
(autoload 'turn-on-vtl-mode "vtl" nil t)
(add-hook 'html-mode-hook 'turn-on-vtl-mode t t)
(add-hook 'xml-mode-hook 'turn-on-vtl-mode t t)
(add-hook 'text-mode-hook 'turn-on-vtl-mode t t)

;; Markdown integration
(autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
(setq auto-mode-alist 
      (append (list '("\\.md$" . markdown-mode)
		    '("\\.[Mm]arkdown$" . markdown-mode))
	      auto-mode-alist))

;; Startup engineering stuff
(require 'ansi-color)
(require 'compile)
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(require 'js-comint)
(defun js-compile ()
  (interactive)
  (compile compile-command))
(setq jshint-cli "jshint --show-non-errors "
      compilation-error-regexp-alist-alist
      (cons '(jshint-cli "^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)"
			 1
			 2
			 3
			 )
	    compilation-error-regexp-alist-alist)
      compilation-error-regexp-alist
      (cons 'jshint-cli compilation-error-regexp-alist))
(add-hook 'js-mode-hook '(lambda ()
			   (local-set-key "\C-x\C-e" 'eval-last-sexp)
			   (local-set-key "\C-cb" 'js-send-buffer)
                           (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                           (local-set-key "\C-cl" 'js-load-file-and-go)
                           (local-set-key "\C-c!" 'run-js)
                           (local-set-key "\C-c\C-r" 'js-send-region)
                           (local-set-key "\C-c\C-j" 'js-send-line)
                           (set (make-local-variable 'compile-command) 
                                (let ((file buffer-file-name)) (concat jshint-cli file)))
                           (set (make-local-variable 'compilation-read-command) nil)
                           (local-set-key "\C-c\C-u" 'js-compile)
                           ))
(defun node-repl-comint-preoutput-filter (output)
  "This function fixes the escape issue with node-repl in js-comint.el.
  Heavily adapted from http://www.squidoo.com/emacs-comint (which
  is in emacs/misc/comint_ticker)

  Basically, by adding this preoutput filter to the
  comint-preoutput-filter-functions list we take the output of
  comint in a *js* buffer and do a find/replace to replace the
  ANSI escape noise with a reasonable prompt.
"
(if (equal (buffer-name) "*js*")
    (progn
      ;; Uncomment these to debug the IO of the node process
      ;; (setq js-node-output output)
      ;; (message (concat "\n----------\n" output "\n----------\n"))

      ;; Replaced ^ with \^ to indicate that doesn't have to be
      ;; at start of line
      (replace-regexp-in-string
       "\\\[0K" ""
       (replace-regexp-in-string
        "\\\[1G" ""
       (replace-regexp-in-string
        "\\\[0J" ""
       (replace-regexp-in-string
        "\\\[3G" ""
       (replace-regexp-in-string
        "\\\[0G" ""
       (replace-regexp-in-string
        "\\[2C" ""
       (replace-regexp-in-string
        "\\[0K" ""
        (replace-regexp-in-string
         "" "" output))))))))
      )
    output
  )
)

(add-hook 'comint-preoutput-filter-functions 'node-repl-comint-preoutput-filter)
(add-hook 'comint-output-filter-functions 'node-repl-comint-preoutput-filter)

;; Enable pandoc for markdown
(add-to-list 'memes-package-archives '("melpa" . "http://stable.melpa.org/packages/"))
(add-to-list 'memes-packages 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(setq markdown-command "pandoc --smart -f markdown -t html")
