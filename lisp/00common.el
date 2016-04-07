;; Settings common to all hosts
;; $Id: 00common.el 2 2007-10-18 23:31:31Z memes $

(setq debug-on-error t	;; Backtrace on error
      memes-hostname	;; Get my hostname from FQDN
      	(substring (system-name) 0
		   (string-match "\\..+" (system-name)))
      inhibit-startup-message t	;; Don't show the startup message
      memes-author-name "memes"
      user-full-name "Matthew Emes"
      default-directory (convert-standard-filename (expand-file-name "~/"))
      ring-bell-function nil	;; Set up a non-audible bell,
      visible-bell t		;; as beeping is really annoying.
      next-line-add-newlines nil;;Don't add new lines to the end of a file
      default-major-mode 'text-mode	;; Open unidentified files in text mode
      require-final-newline nil ;; Don't add a newline to the end of files
      )
;; Allow delete/replace on selected text and highlight selected text
(delete-selection-mode t)
(transient-mark-mode t)

;; Dictionary configuration
(setq ispell-program-name "aspell"
      ispell-skip-html t
      ispell-local-dictionary "british"
      flyspell-use-meta-tab nil
      flyspell-abbrev-p nil
      flyspell-sort-corrections nil)

;; Recognise windows-1252 as latin-1 codepage
(define-coding-system-alias 'windows-1252 'latin-1)

;; Text mode hooks
(add-hook 'text-mode-hook 'turn-on-auto-fill)	;; Turn on auto fill mode
(add-hook 'text-mode-hook 'flyspell-mode)	;; Turn on spell check

;; Set titles for frame and icon (%f == file name, %b == buffer name)
(setq-default frame-title-format (concat memes-hostname ":" "%f"))
(setq-default icon-title-format "%b")

;; Disable the blinking cursor, toolbar and tooltips
(blink-cursor-mode -1)
(tool-bar-mode -1)
(if (fboundp tooltip-mode)
    (tooltip-mode -1)
  )

;; Use line numbering and show the time
(line-number-mode 1)
;(setq display-time-day-and-date t )
;(setq display-time-24hr-format t)
(display-time)

;; Enable uppercase or lowercase conversions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Set key bindings for older versions
(if (fboundp 'pc-bindings-mode)
    (pc-bindings-mode))
(if (fboundp 'pc-selection-mode)
    (pc-selection-mode))
(setq suggest-key-bindings t)
;;(load "cua-mode")
;;(CUA-mode t)

;; Start in home dir
(cd "~")

;; Handle OS specific setup
(cond ((memq window-system '(win32 w32))
       (setq focus-follows-mouse nil))
      ((memq window-system '(ns mac))
       (setq focus-follows-mouse nil))
      (t
       (setq focus-follows-mouse t)))

;; Configure Emacs to use Bash as the shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)
(setq shell-command-switch "-c")
(setenv "SHELL" shell-file-name)
(defun memes-shell-setup()
  "bash under Emacs"
  (setq comint-scroll-show-maximum-output 'this)
  (setq comint-completion-addsuffix t)
  (setq comint-process-echoes nil)
  (setq comint-eol-on-send t)
  (make-variable-buffer-local 'comint-completion-addsuffix)
  (if (memq window-system '(win32 w32))
      (setq w32-quote-process-args ?\")))
(setq shell-mode-hook 'memes-shell-setup)
(cond ((memq window-system '(win32 w32))
       (setq process-coding-system-alist (cons '("bash" . (undecided-dos . undecided-unix))
					       process-coding-system-alist)))
      (t
       (setq process-coding-system-alist (cons '("bash" . raw-text-unix) 
					       process-coding-system-alist))))

;; Quickly revert a file, bound to Alt-r
(defun revert-buffer-noconfirm () 
  "revert buffer with no confirm" 
  (interactive) 
  (revert-buffer t t)
  )
(global-set-key [?\M-r] 'revert-buffer-noconfirm)
(put 'revert-buffer 'disabled nil)

;; Include find file at point
(require 'ffap)
(ffap-bindings)

;; Better identification of buffers containing identically named files in
;; different paths
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Keep a record of recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Whitespace
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualisation" t)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-set-key (kbd "C-x SPC") 'whitespace-mode)

;; Package management - allows list of packages to be added in other config
;; files
(defvar memes-packages ()
  "List of packages to be installed in after-init-hook")
(defvar memes-package-archives ()
  "List of package archive definitions to add to system")
(defvar memes-after-load-packages-hook nil
  "Hook that will be called after loading MELPA packages")
(defun memes-load-packages()
  "Load any packages defined in memes-packages in a hook"
  (when (fboundp 'package-initialize)
    (package-initialize)
    (setq package-archives (append memes-package-archives package-archives))
    (package-refresh-contents)
    (dolist (memes-package memes-packages)
      (unless (package-installed-p memes-package)
	(package-install memes-package)))
    (run-hooks 'memes-after-load-packages-hook)))
(add-hook 'after-init-hook 'memes-load-packages)
(add-hook 'after-init-hook 'memes-init-flycheck)

;; Utility to find a named parent directory
(defun memes-find-parent (memes-name memes-path)
  "Find the parent diretory of memes-path with name memes-name"
  (let ((memes-parent (directory-file-name (file-name-directory memes-path)))
	(memes-check (file-name-nondirectory memes-path)))
    (cond ((equal memes-name memes-check)
	   (string-remove-suffix "/" memes-path))
	  ((equal "/" memes-parent)
	   nil)
	  (t
	   (memes-find-parent memes-name memes-parent)))))
(defun memes-find-first-child-of (memes-name memes-path)
  "Walks the path (virtually) until the parent dir is found. E.g. looking for src in /home/memes/tmp/src/foo/bar/baz will return /home/memes/tmp/src/foo"
  (let* ((memes-parent (directory-file-name (file-name-directory memes-path)))
	(memes-parent-name (file-name-nondirectory memes-parent))
	(memes-check (file-name-nondirectory memes-path)))
    (cond ((equal memes-name memes-check)
	   nil)
	  ((equal "/" memes-parent)
	   nil)
	  ((equal memes-name memes-parent-name)
	   memes-path)
	  (t
	   (memes-find-first-child-of memes-name memes-parent)))))

;; Enable default winmove bindings; Meta-Left/Right/Up/Down to navigate
(windmove-default-keybindings 'meta)

;; Use framemove to extend windmove to frames
(require 'cl)
(add-to-list 'memes-package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'memes-packages 'framemove)
(defun memes-init-framemove ()
  "Initialise framemove after packages have been loaded"
  (require 'framemove)
  (setq framemove-hook-into-windmove t))
(add-hook 'memes-after-load-packages-hook 'memes-init-framemove)
