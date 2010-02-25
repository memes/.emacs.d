;; General coding configuration options
;; $Id: coding-config.el 7 2008-06-27 15:21:24Z memes $

;; Patch/diff mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(setq diff-switches "-Naur")
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; Xref integration
(defun my-load-xrefactory()
  (defvar xref-key-binding 'local)
  (setq exec-path (cons "~/xref" exec-path)
	load-path (cons "~/xref/emacs" load-path))
  (load "xrefactory")
  )
(add-hook 'java-mode-hook 'my-load-xrefactory)

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
