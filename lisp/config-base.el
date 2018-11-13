;;; config-base.el --- base Emacs settings

;;; Commentary:

;; Provides early configuration of Emacs prior to any other customisations.

;;; Code:
(require 'use-config)

;; Basic configuration settings
(setq debug-on-error t
      inhibit-startup-message t
      user-full-name "Matthew Emes"
      default-directory (convert-standard-filename (expand-file-name "~/"))
      ring-bell-function 'ignore
      visible-bell nil
      next-line-add-newlines nil
      require-final-newline nil)

;; File encoding should be UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default buffer-file-coding-system 'utf-8)

;; Text mode defaults
(setq-default fill-column 80
	      indent-tabs-mode nil)

;; Enable uppercase or lowercase conversions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable/disable the GUI elements the way I want
(defun memes/set-gui-elements ()
  "Enable/disable the GUI elements that I want."
  (progn
    (if (fboundp 'menu-bar-mode)
	    (menu-bar-mode -1))
    (if (fboundp 'tool-bar-mode)
	    (tool-bar-mode -1))
    (if (fboundp 'scroll-bar-mode)
	    (scroll-bar-mode 1))
    (if (fboundp 'blink-cursor-mode)
        (blink-cursor-mode -1))))

(use-package text-mode
  :hook
  (text-mode . turn-on-auto-fill))

;; Allow delete/replace on selected text and highlight selected text
(use-package delsel
  :init
  (progn
    (delete-selection-mode t)
    (transient-mark-mode t)))

(use-package iedit
  :ensure t
  :defer t)

;; Spell checking
(defcustom memes/dictionary "british"
  "The dictionary to use for spell checking."
  :type 'string
  :group 'memes/base)

(defcustom memes/lang "en_GB"
  "The language code to use for spell checking."
  :type 'string
  :group 'memes/base)

(use-package ispell
  :defer t
  :config
  (progn
    (setq ispell-silently-savep t
	      ispell-dictionary memes/dictionary)
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell"
	    ispell-extra-args `("--sug-mode=ultra"
			        ,(format "--lang=%s" memes/lang)
			        "--add-filter=url"
			        "--add-filter=email")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell"
	    ispell-extra-args (format "-d %s" memes/lang))))))

(use-package flyspell
  :defer t
  :config
  (validate-setq flyspell-abbrev-p nil
		 flyspell-issue-welcome-flag nil
		 flyspell-issue-message-flag nil
		 flyspell-sort-corrections nil
		 flyspell-use-meta-tab nil)
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package flyspell-lazy
  :ensure t
  :init (after 'flyspell (flyspell-lazy-mode 1)))

(use-package flyspell-correct
  :ensure t
  :config
  (use-package flyspell-correct-ivy
    :ensure t
    :config
    (define-key flyspell-mode-map
      (kbd "C-c $") 'flyspell-correct-previous-word-generic)))

;; Various other configuration settings to delay
(use-package simple
  :init
  (progn
    (when (fboundp 'global-prettify-symbols-mode)
      (add-hook 'after-init-hook #'global-prettify-symbols-mode)))
  :config
  (progn
    (memes/set-gui-elements)
    (setq kill-ring-max most-positive-fixnum)))

;; Make mmm-mode available, but not associated with other modes yet.
(use-package mmm-mode
  :ensure t
  :defer t
  :commands mmm-mode
  :config
  (validate-setq mmm-parse-when-idle t))

;; Helper to load mmm-mode on-demand
;;;###autoload
(defun memes/enable-mmm-mode ()
  "Enable mmm-mode."
  (require 'mmm-mode)
  (mmm-mode 1))

;; Add direnv integration
(use-package direnv
  :ensure t)

(provide 'config-base)
;;; config-base.el ends here
