;;; config-prog-mode.el --- Emacs customisations common to prog modes

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

;; Helper to show trailing whitespace
(defun memes/show-trailing-whitespace ()
  "Enable `SHOW-TRAILING-WHITESPACE` in current buffer."
  (setq-local show-trailing-whitespace t))

(use-package prog-mode
  :defer t
  :config
  (progn
    (add-hook 'prog-mode-hook #'auto-fill-mode)
    (add-hook 'prog-mode-hook #'memes/show-trailing-whitespace)
    (after 'flyspell
      (add-hook 'prog-mode-hook #'flyspell-prog-mode))))



(use-package newcomment
  :defer t
  :config
  (setq-default comment-auto-fill-only-comments t))

(use-package auto-highlight-symbol
  :ensure t
  :config
  (progn
    (validate-setq ahs-case-fold-search nil
		           ahs-default-range 'ahs-range-whole-buffer
		           ahs-idle-interval -1.0
		           ahs-inhibit-face-list '())))

(use-package autoinsert
  :init
  (after-init #'auto-insert-mode))

;; Clean auto-indent mode
(use-package clean-aindent-mode
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'clean-aindent-mode))

;; Trim trailing whitespace
(use-package ws-butler
  :ensure t
  :defer t
  :commands ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; Use dtrt to detect indentation settings - add to all cc-mode derivitives
(use-package dtrt-indent
  :ensure t
  :defer t
  :after cc-mode
  :init
  (add-hook 'cc-mode-hook #'dtrt-indent-mode)
  :config
  (validate-setq dtrt-indent-verbosity 0))

(defvar memes/aggressive-indent-max-lines 100
  "Sets the limit where aggressive-indent will refuse to modify a defun.")

(use-package aggressive-indent
  :ensure t
  :defer t
  :preface
  (defun memes/aggressive-indent-skip-p ()
    "Return true if the current defun is longer than `memes/aggressive-indent-max-lines`."
    (save-excursion
      (ignore-errors
	(let ((b (progn (beginning-of-defun) (line-number-at-pos)))
	      (e (progn (end-of-defun) (line-number-at-pos))))
	  (and b e (<= memes/aggressive-indent-max-lines (- e b)))))))
  :commands aggressive-indent-mode
  :init (after-init #'global-aggressive-indent-mode)
  :config
  (progn
    (validate-setq aggressive-indent-comments-too t)
    ;; Skip large forms
    (add-to-list 'aggressive-indent-dont-indent-if '(memes/aggressive-indent-skip-p))
    ;; Disabled commands
    (dolist (command '(next-line previous-line))
      (add-to-list 'aggressive-indent-protected-commands command))
    ;; Disabled modes
    (dolist (mode '(makefile-mode tuareg-mode cider-repl-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))))

(use-package paren
  :defer t
  :init (add-hook 'prog-mode-hook #'show-paren-mode)
  :preface
  (progn
    (defvar memes/prog-show-paren nil)
    (defun memes/prog-show-paren-turn-off ()
      (when (bound-and-true-p show-paren-mode)
	(setq memes/prog-show-paren t)
	(show-paren-mode -1))))
  :config
  (progn
    (validate-setq show-paren-style 'expression
		   show-paren-priority 1000
		   show-paren-delay 0.05)
    (add-hook 'activate-mark-hook #'(lambda () (show-paren-mode -1)))
    (add-hook 'deactivate-mark-hook #'(lambda () (show-paren-mode 1)))))

(use-package paredit
  :ensure t
  :defer t
  :commands paredit-mode
  :functions
  (paredit-wrap-round paredit-wrap-square paredit-wrap-curly)
  :commands
  (paredit-raise-sexp
   paredit-backward-barf-sexp
   paredit-forward-barf-sexp
   paredit-forward-slurp-sexp
   paredit-backward-slurp-sexp
   paredit-wrap-round
   paredit-wrap-square
   paredit-wrap-curly
   paredit-kill
   paredit-splice-sexp))

;; Hook ivy into xref
(use-package ivy-xref
  :ensure t
  :defer t
  :commands ivy-xref-show-xrefs
  :after ivy
  :init
  (validate-setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'config-prog-mode)
;;; config-prog-mode.el ends here
