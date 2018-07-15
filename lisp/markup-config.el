;;; markup-config.el --- Markup editing configuration

;;; Commentary:

;;; Code:

(defvar memes-packages)

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
(defvar memes-schema-locating-file
  (locate-user-emacs-file "schemas/schemas.xml")
  "Location for other schema references.")
(setq-default rng-schema-locating-files (append (list memes-schema-locating-file)
						rng-schema-locating-files
						)
	      nxml-slash-auto-complete-flag t
	      nxml-sexp-element-flag t)

;; Load Norman Walsh's unicode support libraries
(add-to-list 'memes-packages 'xmlunicode)

;; Stolen from "drkm" post on nxml list
(defun surround-region-with-tag (tag-name beg end)
  "Add a tag to selected content."
  (interactive "sTag name: \nr")
  (save-excursion
    (goto-char beg)
    (insert "<" tag-name ">")
    (goto-char (+ end 2 (length tag-name)))
    (insert "</" tag-name ">")))

;; Define the nxml entry config
(defun memes-nxml-mode-hook ()
  "Prepare nXML mode."
  (require 'xmlunicode)
  (auto-fill-mode t)
  (set-fill-column 78)
  (flyspell-mode)
  (set-language-environment "utf-8")
  (define-key nxml-mode-map "\C-r" 'surround-region-with-tag)
  (define-key nxml-mode-map [\C-tab] 'nxml-complete)
  (define-key nxml-mode-map "\"" 'xmlunicode-smart-double-quote)
  (define-key nxml-mode-map "\'" 'xmlunicode-smart-single-quote)
  (define-key nxml-mode-map "\-" 'xmlunicode-smart-hyphen)
  (define-key nxml-mode-map "\." 'xmlunicode-smart-period)
  (define-key nxml-mode-map [menu-bar unichar]
	      (cons "UniChar" xmlunicode-character-menu-map))
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
(require 'cl-lib)
(defun hexcolour-luminance (colour)
  "Calculate the luminance of COLOUR string (e.g. \"#ffaa00\", \"blue\").
This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values colour))
	 (r (car values))
	 (g (cadr values))
	 (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))
(defun hexcolour-add-to-font-lock ()
  "Change the font-face colour to match the value."
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
(defun memes-css-mode-hook()
  "Prepare for css-mode"
  (setq-default css-indent-offset 2)
  (hexcolour-add-to-font-lock))
(add-hook 'css-mode-hook 'memes-css-mode-hook)

;; Markdown integration
(add-to-list 'memes-packages 'markdown-mode)

;; Enable pandoc for markdown
(add-to-list 'memes-packages 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(setq-default markdown-command "pandoc -f markdown -t html+smart")

;; Add web-mode
(add-to-list 'memes-packages 'web-mode)
(add-to-list 'memes-packages 'company-web)
(add-to-list 'auto-mode-alist '("\\.\\(html?\\|php\\|jsp\\|aspx?\\|cshtml\\|[jt]sx\\|vtl\\)\\'" . web-mode))
(defun memes-web-mode-hook ()
  "Prepare web-mode for use."
  (setq web-mode-markup-indent-offset 2
		web-mode-css-indent-offset 2
		web-mode-code-indent-offset 2
                indent-tabs-mode nil
		web-mode-enable-engine-detection t)
  (set (make-local-variable 'company-backends) '(company-web-html company-files))
  ;; Enable tide in tsx and jsx files
  (when (string-match "[jt]sx" (file-name-extension buffer-file-name))
    (memes-tide-mode-hook)))
(add-hook 'web-mode-hook 'memes-web-mode-hook)
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Handle embedded jsx content."
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
	ad-do-it)
    ad-do-it))

;; LESS mode
(add-to-list 'memes-packages 'less-css-mode)
(with-eval-after-load "less-css-mode"
  (setq less-css-compile-at-save nil))

;; YAML mode, with ansible completion
(add-to-list 'memes-packages 'yaml-mode)
(add-to-list 'memes-packages 'company-ansible)
(defun memes-yaml-mode-hook ()
  "Configure YAML mode."
  (add-to-list 'company-backends 'company-ansible)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))
(add-hook 'yaml-mode-hook 'memes-yaml-mode-hook)
(add-to-list 'auto-mode-alist '("\\.raml\\'" . yaml-mode))

;; TOML mode
(add-to-list 'memes-packages 'toml-mode)

(provide 'markup-config)
;;; markup-config.el ends here
