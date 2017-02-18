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

;; Configure flycheck after initialisation is compelete
(defun memes-init-flycheck ()
  "Turn on flycheck everywhere"
  (global-flycheck-mode)
  ;; Disable jshint and json checkers
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers '(javascript-jshint json-jsonlint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode))
(add-hook 'memes-after-load-packages-hook 'memes-init-flycheck)

;; Set C-x c to launch compile command
(setq compilation-read-command nil)
(global-set-key "\C-xc" 'compile)

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
(add-to-list 'memes-packages 'scala-mode)

;; Clojure package
(add-to-list 'memes-packages 'clojure-mode)
(with-eval-after-load "clojure-mode"
  (add-to-list 'memes-paredit-mode-hooks clojure-mode-hook))
(add-to-list 'memes-packages 'cider)
(with-eval-after-load "cider-mode"
  (add-hook 'cider-mode-hook #'eldoc-mode))

;; Go language support
(add-to-list 'memes-packages 'go-mode)
(add-to-list 'memes-packages 'exec-path-from-shell)
(add-to-list 'memes-packages 'go-eldoc)
(add-to-list 'memes-packages 'go-errcheck)
(add-to-list 'memes-packages 'go-rename)
(add-to-list 'memes-packages 'go-guru)
(defconst memes-goroot
  (convert-standard-filename (expand-file-name
			      (cond ((memq window-system '(w32 win32)) "~/go")
				    ((memq window-system '(ns mac)) "~/Library/go")
				    (t "~/lib/go"))))
  "Local root of projects - separate OS go libs from manually installed")
(defun memes-gb-project-path (filename)
  "Returns the gb project path for filename or nil"
  (let ((gb-info-results (shell-command-to-string (format "cd %s && gb info" (directory-file-name (file-name-directory filename))))))
    (if (string-match "GB_PROJECT_DIR=\"\\(.*\\)\"" gb-info-results)
	(let ((gb-project-path (match-string 1 gb-info-results)))
	  (if (and (file-directory-p (concat gb-project-path "/src"))
		   (file-directory-p (concat gb-project-path "/vendor")))
	      gb-project-path
	    nil))
      nil)))
(defun memes-go-compile ()
  "Returns a string of shell commands to compile current project"
  (let ((gb-project-path (memes-gb-project-path buffer-file-name)))
    (if gb-project-path
	(format "cd %s && gb build && gb test -v=1 && go tool vet %s/src" gb-project-path gb-project-path)
      (let ((go-project-path (or (directory-file-name (file-name-directory (memes-find-parent "src" buffer-file-name)))
				 (directory-file-name (file-name-directory buffer-file-name)))))
	(format "cd %s && go build -v ./... && go test -v=1 && go tool vet ." go-project-path)))))
(defun memes-go-mode-hook ()
  "Hook to be executed in all go buffers"
  (require 'exec-path-from-shell)
  (require 'go-autocomplete (convert-standard-filename (concat memes-goroot "/src/github.com/nsf/gocode/emacs/go-autocomplete.el")))
  ;;(require 'go-oracle (convert-standard-filename (concat memes-goroot "/src/golang.org/x/tools/cmd/oracle/oracle.el")))
  (setq gofmt-command "goimports")
  (go-eldoc-setup)
  (make-local-variable 'process-environment)
  (setenv "GOPATH"
	  (mapconcat 'identity
		     (append (split-string (or (go-guess-gopath) "") path-separator) (list memes-goroot))
		     path-separator))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c r") 'go-rename)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'compile-command) (memes-go-compile))
  (go-guru-hl-identifier-mode)
  )
(add-hook 'go-mode-hook 'memes-go-mode-hook)

;; Javascript support
(add-to-list 'memes-packages 'js2-mode)
(add-to-list 'memes-packages 'ac-js2)
(defun memes-js-mode-hook ()
  "Hook to be executed for js modes"
  (js2-minor-mode)
  (ac-js2-mode)
  (setq js-indent-level 2
        js2-basic-offset 2
        js2-bounce-indent-p t
	indent-tabs-mode nil))
(add-hook 'js-mode-hook 'memes-js-mode-hook)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Stolen from http://stackoverflow.com/a/35806330 - better JSON support in js2-mode
(make-variable-buffer-local 'js2-parse-as-json)
(defadvice js2-reparse (before json)
  (setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)
(defadvice js2-parse-statement (around json)
  (if (and (= tt js2-LC)
	   js2-buffer-file-name
	   (or js2-parse-as-json
	       (string-equal (substring js2-buffer-file-name -5) ".json"))
	   (eq (+ (save-excursion
		    (goto-char (point-min))
		    (back-to-indentation)
		    (while (eolp)
		      (next-line)
		      (back-to-indentation))
		    (point)) 1) js2-ts-cursor))
      (setq ad-return-value (js2-parse-assign-expr))
    ad-do-it))
(ad-activate 'js2-parse-statement)
(define-derived-mode json-mode js2-mode "JSON"
  "Major mode for editing JSON data."
  :group 'json
  (setq js2-parse-as-json t)
  (js2-reparse t))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; Coffee script support
(add-to-list 'memes-packages 'coffee-mode)
(defun memes-coffee-mode-hook ()
  "Hook to be executed for coffee-mode"
  (coffee-tab-width 2))
(add-hook 'coffee-mode-hook 'memes-coffee-mode-hook)

;; TypeScript support
(add-to-list 'memes-packages 'tide)
(defun memes-tide-mode-hook ()
  "Hook executed for typescript-mode"
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (add-hook 'before-save-hook 'tide-format-before-save nil t))
(add-hook 'typescript-mode-hook #'memes-tide-mode-hook)
(setq typescript-indent-level 2
      tide-format-options
      '(:tabSize 2 
	:indentSize 2
	:insertSpaceAfterCommaDelimiter t
	:insertSpaceAfterSemicolonInForStatements t
	:insertSpaceBeforeAndAfterBinaryOperators t
	:insertSpaceAfterKeywordsInControlFlowStatements t
	:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
	:insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis nil
	:insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets nil
	:insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil
	:insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces nil
	:placeOpenBraceOnNewLineForControlBlocks nil
	:placeOpenBraceOnNewLineForFunctions nil))

;; C# mode
(add-to-list 'memes-packages 'csharp-mode)
(defun memes-csharp-mode-hook ()
  "C# hook"
  (electric-pair-mode 1))
(add-hook 'csharp-mode-hook 'memes-csharp-mode-hook)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Swift support
(add-to-list 'memes-packages 'swift-mode)
(defun memes-swift-mode-hook ()
  "Swift hook"
  (add-to-list 'flycheck-checkers 'swift)
  (setq flycheck-swift-target nil
	swift-repl-executable (cond ((memq window-system '(ns mac)) "xcrun swift")
				    (t "swift"))))
(add-hook 'swift-mode-hook 'memes-swift-mode-hook)

;; Protobuf support
(add-to-list 'memes-packages 'protobuf-mode)
(defun memes-protobuf-mode-hook ()
  "Protobuf hook"
  (add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t))
(add-hook 'protobuf-mode-hook 'memes-protobuf-mode-hook)

;; Angular 2 support
(add-to-list 'memes-packages 'ng2-mode)
(add-hook 'ng2-mode-hook 'memes-tide-mode-hook)
(defun memes-ng2-html-mode-hook ()
  "Hook for ng2 HTML mode"
  (auto-fill-mode 0))
(add-hook 'ng2-html-mode-hook 'memes-ng2-html-mode-hook)
