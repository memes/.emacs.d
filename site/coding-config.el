;; General coding configuration options
;; $Id: coding-config.el 7 2008-06-27 15:21:24Z memes $

;; Patch/diff mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(setq diff-switches "-Naur")
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; Xref integration - disabled since no JDK <1.5 for amd64
(defun my-load-xrefactory()
  (defvar xref-key-binding 'local)
  (setq exec-path (cons "~/xref" exec-path)
	load-path (cons "~/xref/emacs" load-path))
  (load "xrefactory")
  )
;;(add-hook 'java-mode-hook 'my-load-xrefactory)

;; Default c-mode settings
(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'case-label '+)
  (setq-default tab-width 4 indent-tabs-mode nil)
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (message "my-c-mode-common-hook function executed"))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Set linux mode
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
        (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match "~/dev/kernel" filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux")
                (c-set-offset 'arglist-cont-nonempty
                              '(c-lineup-gcc-asm-reg
                                c-lineup-arglist-tabs-only))))))


;; Show matching parenthesis
(show-paren-mode t)

;; Recognise Ant and Jikes errors
(require 'compile)
(setq compilation-error-regexp-alist 
      (append (list
	       ;; works for jikes
	       '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
	       ;; works for javac
	       '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
	      compilation-error-regexp-alist))

;; C# Mode - from Beagle's HACKING doc
(defun poor-mans-csharp-mode ()
  (java-mode)
  (setq mode-name "C#")
  (set-variable 'tab-width 8)
  (set-variable 'indent-tabs-mode t)
  (set-variable 'c-basic-offset 8)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label 0)
)

(setq auto-mode-alist (append '(("\\.cs\\'" . poor-mans-csharp-mode))
			      auto-mode-alist))

;; Flymake configuration - add integration for python and use Ant for Java
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (setq flymake-allowed-file-name-masks
        '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
;;          ("\\.xml\\'" flymake-xml-init)
;;          ("\\.html?\\'" flymake-xml-init)
          ("\\.cs\\'" flymake-simple-make-init)
          ("\\.p[ml]\\'" flymake-perl-init)
          ("\\.php[345]?\\'" flymake-php-init)
          ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
;;          ("\\.java\\'" flymake-simple-ant-java-init flymake-simple-java-cleanup)
          ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
          ("\\.tex\\'" flymake-simple-tex-init)
          ("\\.idl\\'" flymake-simple-make-init))))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Python integration 
(add-hook 'python-mode-hook
	  (lambda ()
	    (unless (eq buffer-file-name nil) (flymake-mode 1)) ; don't invoke flymake in interpreter buffers
	    (local-set-key [f2] 'flymake-goto-prev-error)
	    (local-set-key [f3] 'flymake-goto-next-error)
	    ))

;; Groovy/Grails integration
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-hook 'groovy-mode-hook
	  '(lambda ()
	     (require 'groovy-electric)
	     (groovy-electric-mode)))