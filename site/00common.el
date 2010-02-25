;; Settings common to all hosts
;; $Id: 00common.el 2 2007-10-18 23:31:31Z memes $

(setq debug-on-error t	;; Backtrace on error
      my-hostname	;; Get my hostname from FQDN
      	(substring (system-name) 0
		   (string-match "\\..+" (system-name)))
      inhibit-startup-message t	;; Don't show the startup message
      my-author-name "memes"
      user-full-name "Matthew Emes"
      default-directory "~/"
      ring-bell-function nil	;; Set up a non-audible bell,
      visible-bell t		;; as beeping is really annoying.
      transient-mark-mode '1	;; Show mark'ed text
      next-line-add-newlines nil;;Don't add new lines to the end of a file
      default-major-mode 'text-mode	;; Open unidentified files in text mode
      require-final-newline nil ;; Don't add a newline to the end of files
      )
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
(setq-default frame-title-format (concat my-hostname ":" "%f"))
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

;; Set key bindings
(pc-bindings-mode)
(pc-selection-mode)
;;(load "cua-mode")
;;(CUA-mode t)

;; Start in home dir
(cd "~")

;; Configure Emacs to use Bash as the shell
(setq shell-file-name "bash")
(setq explicit-shell-file-name shell-file-name)
(setq shell-command-switch "-c")
(setenv "SHELL" shell-file-name)
(defun my-shell-setup()
  "bash under Emacs 20"
  (setq comint-scroll-show-maximum-output 'this)
  (setq comint-completion-addsuffix t)
  (setq comint-process-echoes nil)
  (setq comint-eol-on-send t)
  (make-variable-buffer-local 'comint-completion-addsuffix))
(setq shell-mode-hook 'my-shell-setup)
(setq process-coding-system-alist (cons '("bash" . raw-text-unix) 
					process-coding-system-alist))

;; A quick goto-line hack... bound to Alt-g
(global-set-key "\M-g" 'goto-line)

;; Quickly revert a file, bound to Alt-r
(defun revert-buffer-noconfirm () 
  "revert buffer with no confirm" 
  (interactive) 
  (revert-buffer t t)
  )
(global-set-key [?\M-r] 'revert-buffer-noconfirm)
(put 'revert-buffer 'disabled nil)

;; Function to create a new frame and execute command in it
(defun my-run-command-new-frame (command)
  "Run command in a new frame."
  (select-frame (make-frame))
  (call-interactively command))

(defun memes-ltrim (string &optional what-to-trim)
  "Return STRING with any whitespace trimmed from the left.
If WHAT-TO-TRIM is non-nil, use the chars in it instead of whitespace."
  (save-match-data
    (if (string-match (format "^[%s]+" (or what-to-trim "\t ")) string)
	(substring string (match-end 0) nil)
      string)))

(defun memes-rtrim (string &optional what-to-trim)
  "Return STRING with any whitespace trimmed from the right.
If WHAT-TO-TRIM is non-nil, use the chars in it instead of whitespace."
  (save-match-data
    (if (string-match (format "[%s]+$" (or what-to-trim "\t ")) string)
	(substring string 0 (match-beginning 0))
      string)))

(defun memes-trim (string &optional what-to-trim)
  "Return STRING with any whitespace trimmed from the left & right.
If WHAT-TO-TRIM is non-nil, use the chars in it instead of whitespace."
  (save-match-data
    (if string
	(memes-ltrim (memes-rtrim string what-to-trim) what-to-trim)
      string)))
