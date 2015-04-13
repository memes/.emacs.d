;; Options used when editing various markup languages
;; $Id: markup-config.el 5 2007-10-20 23:05:29Z memes $

;; Configure Flyspell for xml/nxml modes
(put 'xml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
(put 'nxml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)

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
(defun memes-nxml-mode-hook ()
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
(add-hook 'nxml-mode-hook 'memes-nxml-mode-hook)
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

;; VTL mode for velocity
(autoload 'turn-on-vtl-mode "vtl" nil t)
(add-hook 'xml-mode-hook 'turn-on-vtl-mode t t)
(add-hook 'text-mode-hook 'turn-on-vtl-mode t t)

;; Markdown integration
(autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
(setq auto-mode-alist 
      (append (list '("\\.md$" . markdown-mode)
		    '("\\.[Mm]arkdown$" . markdown-mode))
	      auto-mode-alist))


;; Enable pandoc for markdown
(add-to-list 'memes-package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'memes-packages 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(setq markdown-command "pandoc --smart -f markdown -t html")

;; Add web-mode
(add-to-list 'memes-packages 'web-mode)
(add-to-list 'auto-mode-alist '("\\.\\(html?\\|php\\|jsp\\|aspx?\\|cshtml\\|jsx\\)\\'" . web-mode))
(defun memes-web-mode-hook ()
  "Prepare web-mode for use"
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (auto-complete-mode 1)
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-words-in-buffer ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))
	  ("jsx" . (ac-source-words-in-buffer ac-source-words-in-same-mode-buffers)))))
(add-hook 'web-mode-hook 'memes-web-mode-hook)
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
	ad-do-it)
    ad-do-it))
