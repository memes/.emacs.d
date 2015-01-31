;; General coding configuration options
;; $Id: coding-config.el 7 2008-06-27 15:21:24Z memes $

;; Show matching parenthesis where possible
(show-paren-mode 1)

;; List of hooks to add paredit support
;;  - add to this list when modes should hook paredit
(defvar memes-paredit-mode-hooks
  '(emacs-lisp-mode-hook lisp-mode-hook)
  "Hooks to include paredit support")

;; Enable paredit for list like modes
(with-eval-after-load "paredit"
  ;; Don't override M-r since I use it too often
  (define-key paredit-mode-map (kbd "<M-r>" nil))
  ;; Add paredit mode to all hooks in memes-paredit-mode-hooks
  (mapc (lambda (hook)
	  (add-hook hook (lambda ()
			   (paredit-mode +1))))
	memes-paredit-mode-hooks))

;; Patch/diff mode if installed on OS
(cond ((fboundp 'diff-mode)
       ;; Autoload diff mode for diff/patch like files
       (autoload 'diff-mode "diff-mode" "Diff major mode" t)
       (setq diff-switches "-Naur")
       (add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))))

;; Add flycheck to all supported languages
(add-to-list 'memes-package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'memes-packages 'flycheck)
(with-eval-after-load "flycheck"
  (add-hook 'emacs-startup-hook 'global-flycheck-mode))

;; C-mode hook common to all sub-modes
(defun memes-c-mode-common-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'case-label '+)
  (setq-default tab-width 4 indent-tabs-mode nil)
  (turn-on-auto-fill)
  (flyspell-prog-mode))
(add-hook 'c-mode-common-hook 'memes-c-mode-common-hook)

;; Define a set of path/c-style option pairs
(setq memes-c-style-alist
      '(("~/dev/kernel" . "linux-tabs-only")
	(nil . "k&r")))

;; Function to pick a c-style based on filepath
(defun memes-choose-c-style ()
  "Choose a C-style based on filepath of buffer"
  (let ((style
	 (assoc-default buffer-file-name memes-c-style-alist
			(lambda (pattern path)
			  (or (not pattern)
			      (and (stringp path)
				   (string-match (expand-file-name pattern)
						 path))))
			;; Add a default in case there are no matches
			'(nil . "k&r"))))
    (cond
     ((stringp style) (c-set-style style))
     ((functionp style) (style)))))
(add-hook 'c-mode-hook 'memes-choose-c-style)

;; Setup Linux style - mostly from Documentation/CodingStyle
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))
(defconst linux-tabs-only
  '("linux"
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "Linux CodingStyle recommendations")
(c-add-style "linux-tabs-only" linux-tabs-only)

;; Groovy/Grails integration
(add-to-list 'memes-packages 'groovy-mode)
(with-eval-after-load "groovy-mode"
  (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
  (add-hook 'groovy-mode-hook
	    '(lambda ()
	       (require 'groovy-electric)
	       (groovy-electric-mode))))

;; Lua mode if installed OS package
(cond ((fboundp 'lua-mode)
       (autoload 'lua-mode "lua-mode" "Lua editing mode" t)
       (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
       (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

;; Use Octave for .m files in preference to MATLAB when OS package is installed
(cond ((fboundp 'octave-mode)
       (setq octave-block-offset 4)
       (setq auto-mode-alist (cons '("\\.m\\'" . octave-mode) auto-mode-alist))))

;; Load scala from package
(add-to-list 'memes-packages 'scala-mode2)

;; Clojure package
(add-to-list 'memes-packages 'clojure-mode)
(with-eval-after-load "clojure-mode"
  (add-to-list 'memes-paredit-mode-hooks clojure-mode-hook))

;; Go language support
(add-to-list 'memes-packages 'go-mode)
(defun memes-go-mode-hook ()
  "Hook to be executed in all go buffers"
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") 'go-goto-imports))
(add-hook 'go-mode-hook 'memes-go-mode-hook)

;; Javascript support
(add-to-list 'memes-packages 'js2-mode)
(add-to-list 'memes-packages 'ac-js2)
(defun memes-js-mode-hook ()
  "Hook to be executed for js modes"
  (js2-minor-mode)
  (ac-js2-mode))
(add-hook 'js-mode-hook 'memes-js-mode-hook)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
