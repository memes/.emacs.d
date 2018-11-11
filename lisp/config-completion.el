:;; config-completion.el --- Emacs auto-completion setup

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)
(require 'map)

(defcustom memes/completion-enable-yasnippet t
  "Enable yasnippet for all backends."
  :type 'boolean
  :group 'memes/completion)

(defvar memes/completion-backends-alist
  '((emacs-lisp-mode
     . (company-elisp
	company-files
	company-dabbrev-code
	company-keywords
	company-dabbrev))))

(defvar memes/completion-default-backends
  '(company-capf
    company-files
    (company-dabbrev-code company-keywords)))


;; Defaults
(setq-default tab-always-indent 'complete
	      completion-styles '(partial-completion substring basic))

(use-package abbrev
  :defer t
  :if (file-exists-p abbrev-file-name)
  :config
  (progn
    (setq-default abbrev-mode t)
    (validate-setq save-abbrevs 'silently)
    (quietly-read-abbrev-file)))

(use-package smart-tab
  :ensure t
  :defer t)

(defun memes/completion-add-backends (mode &rest backends)
  "Add 'MODE' specific 'BACKENDS' to 'memes/completion-backends-alist'."
  (let* ((existing (map-elt memes/completion-backends-alist mode)))
    (map-put memes/completion-backends-alist mode (append existing backends))))

(defun memes/company-backends ()
  (cl-reduce
   (lambda (acc mode)
     (if-let ((backend (alist-get mode memes/completion-backends-alist)))
         (cons backend acc)
       acc))
   (memes/completion-enabled-modes)
   :initial-value
   (if memes/completion-enable-yasnippet '(company-yasnippet) '())))

(use-package company
  :ensure t
  :defer t
  :commands (company-mode company-complete-common-or-cycle)
  :preface
  (defun memes/completion-enabled-minor-modes ()
    (cl-remove-if-not
     (lambda (mode)
       (and (boundp mode) (symbol-value mode)))
     minor-mode-list))

  (defun memes/completion-enabled-modes ()
    (cons major-mode (memes/completion-enabled-minor-modes)))

  (defun memes/completion-company-backends ()
    (cl-reduce
     (lambda (acc mode)
       (if-let ((backend (alist-get mode memes/completion-backends-alist)))
           (cons backend acc)
         acc))
     (memes/completion-enabled-modes)
     :initial-value '()))

     (defun memes/completion-backend-with-yasnippet (backend)
    ;; Avoid double-wrapping
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defun memes/completion-company-turn-on ()
    (let ((backends (append (memes/completion-company-backends) memes/completion-default-backends)))
      ;; Set company backends, conditionally enabling yasnippet
      (setq company-backends
            (if memes/completion-enable-yasnippet
                (mapcar #'memes/completion-backend-with-yasnippet backends)
              backends))
      ;; Make smart-tab use company-mode
      (setq-local smart-tab-completion-functions-alist
                  `((,major-mode . company-complete-common)))
      ;; Smart-tab is our completion entry point
      (smart-tab-mode 1)
      (company-mode 1)))
  :init (add-hook 'prog-mode-hook 'memes/completion-company-turn-on)
  :config
  (progn
    (bind-key "TAB" #'company-complete-common-or-cycle company-active-map)
    (validate-setq company-idle-delay nil
		   company-minimum-prefix-length 2
		   company-tooltip-align-annotations t
		   company-require-match nil)))

(use-package company-elisp
  :defer t
  :config
  (validate-setq company-elisp-detect-function-context nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init
  (after 'company
    (bind-key "C-h" 'company-quickhelp-mode company-active-map)))

(use-package company-statistics
  :ensure t
  :defer t
  :init
  (after 'company
    (add-hook 'company-mode-hook #'company-statistics-mode))
  :config
  (validate-setq company-statistics-file (memes/var "company-statistics.el")
		 company-statistics-size 200))

(use-package yasnippet
  :if memes/completion-enable-yasnippet
  :ensure t
  :defer t
  :init (after-init #'yas-global-mode)
  :config
  (progn
    (unbind-key "<tab>" yas-minor-mode-map)
    (unbind-key "TAB" yas-minor-mode-map)
    (unbind-key "C-c <tab>" yas-minor-mode-map)
    (unbind-key "C-c TAB" yas-minor-mode-map)
    (add-to-list 'yas-snippet-dirs (memes/file "snippets/"))
    (validate-setq yas-fallback-behavior 'return-nil
		   yas-triggers-in-field t
		   yas-verbosity 0)))

(use-package swiper
  :ensure t
  :defer t)

(use-package uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package ivy
  :init (after-init #'ivy-mode)
  :commands (ivy-set-actions)
  :config
  (progn
    (validate-setq ivy-initial-inputs-alist nil
		   ivy-re-builders-alist '((t . ivy--regex-ignore-order))
		   ivy-use-virtual-buffers t
		   ivy-virtual-abbreviate 'full)))

(use-package counsel
  :ensure t
  :init (after-init #'counsel-mode)
  :preface
  (progn
    (defun memes/counsel-delete-file (x)
      (delete-file (expand-file-name x ivy--directory)))
    (defun memes/counsel-find-file-other-window (x)
      (find-file-other-window (expand-file-name x ivy--directory))))
  :config
  (progn
    (ivy-set-actions
     'counsel-find-file `(("x" memes/counsel-delete-file ,(propertize "delete" 'face 'font-lock-warning-face))
			  ("4" memes/counsel-find-file-other-window "other-window")))
    (validate-setq counsel-find-file-at-point t
		   ivy-extra-directories nil)))

(use-package smex
  :ensure t
  :defer t
  :init
  (progn
    (defvar smex-history-length 100)
    (defvar smex-save-file (memes/var "smex"))))

(use-package counsel-projectile
  :ensure t
  :defer t
  :after projectile
  :init (counsel-projectile-mode))

(use-package ivy-historian
  :ensure t
  :after ivy
  :config
  (progn
    (validate-setq historian-save-file (memes/var "historian"))
    (ivy-historian-mode +1)))

(after 'counsel
  (defvar memes/counsel-git-grep-todos-cmd "git --no-pager grep --full-name -n --no-color -e TODO -e FIXME -e FIX -e HACK -e XXX -e XXXX -e FAIL -e ERROR")

  (defun memes/counsel-git-grep-project-todos ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (counsel-git-grep memes/counsel-git-grep-todos-cmd))))

(provide 'config-completion)
;;; config-completion.el ends here
