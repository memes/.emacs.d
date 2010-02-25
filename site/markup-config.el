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
				 '("~/emacs/schemas/schemas.xml")
				 rng-schema-locating-files
				 )
      nxml-slash-auto-complete-flag t
      nxml-sexp-element-flag t)

;; Load Norman Walsh's unicode support libraries
(require 'cl)
(setq unicode-character-list-file "~/emacs/site/unichars.el")
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

;; Load nXhtml mode
(load "/home/memes/emacs/site/nxhtml/autostart.el")
