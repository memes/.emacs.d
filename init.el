;;; init.el --- initialise Emacs

;;

;;; Commentary:

;; Rework of Emacs initialisation, inspiration from
;; https://github.com/julienfantin/.emacs.d

;;; Code:

;; Latest customisations require Emacs26; I don't have anything older than that
;; installed to test anywhere.
(let ((min_supported 26))
  (unless (>= emacs-major-version min_supported)
    (error "Emacs %d or later is required for this configuration" min_supported)))

;; Enable error reporting during startup,but disable after initialisation
(setq debug-on-error t)
(add-hook 'after-init-hook (lambda () (setq debug-on-error nil)))

;; Base requirements
(require 'cl-lib)
(require 'cl-macs)
(require 'package)

;; Speed up initial startup by tweaking params, but reset after load.
(defvar memes/file-name-handler-alist file-name-handler-alist)
(defvar memes/gc-cons-threshold gc-cons-threshold)
(setq file-name-handler-alist nil
      gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    "Restore default values after init."
	    (setq file-name-handler-alist memes/file-name-handler-alist
		  gc-cons-threshold memes/gc-cons-threshold)
	    (add-hook 'focus-out-hook 'garbage-collect)))

;; Configure package repos.
(eval-and-compile
  ;; Prepare paths, add utility functions for local lisp files
  (load-file (expand-file-name "lisp/config-path.el" user-emacs-directory))
  ;; Setup package repos
  (load-file (expand-file-name "lisp/config-packages.el" user-emacs-directory))
  ;; Use Julien Fantin's use-config
  ;;See https://github.com/julienfantin/.emacs.d/blob/master/lib/use-config/use-config.el
  (load-file (expand-file-name "lisp/use-config.el" user-emacs-directory)))

;; Make sure the use-package is loaded and good to go
(require 'use-package)

;; Bring in bug-hunter explicitly so that errors from my bad lisp code can be
;; detected early.
(use-package bug-hunter
  :ensure t)

;; All my systems can use this now, so just load it!
(use-package exec-path-from-shell
  :ensure t
  :init
  (progn
    (exec-path-from-shell-initialize)))

;; Make sure that the local use-config module is loaded and initialised
(use-package use-config
  :demand t)

;; Make sure the path functions are available
(use-config config-path)

;; Load configurations order is now explicit and not tied to hostname or other
;; crap.

;; Load base configuration; text mode settings, etc.
(use-config config-base)

;; Deal with any platform specific setup
(use-config config-platform)

;; File handling
(use-config config-files)

;; Completion settings
(use-config config-completion)

;; Searching in buffer
(use-config config-search)

;; Load a theme
(use-config config-colour-scheme)

;; Integrate shell support
(use-config config-shell)

;; Browser integration
(use-config config-browser)

;; Eldoc
(use-config config-eldoc)

;; Git configuration
(use-config config-git)

;; IRC - still used occasionally
(use-config config-irc)

;; Prog mode
(use-config config-prog-mode)

;; Flycheck
(use-config config-flycheck)

;; Enable semantic mode
(use-config config-semantic)

;; Enable project management
(use-config config-projects)

;; Window customisations for terminal and window systems
(use-config config-buffer)

;; Window/buffer management
(use-config config-windows)

;; If running on a window system, load any customisations
(use-config config-window-system)

;; Now load the modes used for coding, etc.

;; YAML support
(use-config config-yaml)

;; Docker support
(use-config config-docker)

;; Makefile support
(use-config config-makefile)

;; Lisp - mostly for emacs
(use-config config-lisp)

;; Languge server protocol support
(use-config config-lsp)

;; C/C++ support
(use-config config-c-cpp)

;; Groovy/Grails
(use-config config-groovy)

;; Lua
(use-config config-lua)

;; Clojure
(use-config config-clojure)

;; Go
(use-config config-go)

;; Javascript/node support, and related languages
(use-config config-javascript)
(use-config config-json)

;; Swift
(use-config config-swift)

;; Protobuf
(use-config config-protobuf)

;; Elixir
(use-config config-elixir)

;; Java
(use-config config-java)

;; Python
(use-config config-python)

;; Dart
(use-config config-dart)

;; SQL
(use-config config-sql)

;; CSS/HTML file support
(use-config config-css-html)

;; Markdown
(use-config config-markdown)

;; Ansible support
(use-config config-ansible)

;; Terraform support
(use-config config-terraform)

;; Set keybindings last
(use-config config-keybindings)

(provide 'init)
;;; init.el ends here
