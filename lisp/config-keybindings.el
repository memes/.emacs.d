;;; config-keybindings.el --- Emacs keybindings

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)
(require 'cl-lib)

;; Which function will be used to setup keyboard?
(defvar memes/kbd-setup #'memes/default-kbd-modifiers)

;; Default set of keyboard modifiers is platform dependent
(defun memes/default-kbd-modifiers ()
  "Set any required modifiers for this system."
  (cl-case system-type
    ('darwin (
              validate-setq mac-function-modifier 'hyper))))

;; If memes/kbd-setup is valid, execute the function
(when memes/kbd-setup
  (funcall memes/kbd-setup))

;; If true, the hyper key will be mapped to C-c
(defvar memes/map-hyper-to-C-c t)

(defvar memes/keybindings-to-remap
  (cl-list* "<tab>"
            "RET"
            (string-to-list
             "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-={}{};'\\:\"|,./<>?~+"))
  "The list of keys that are permitted to be rebinded to hyper.")

;; Perform a remap of C-c to hyper
(defun memes/remap-hyper-to-C-c ()
  "Remap hyper single key bindings to C-c.

This allows quicker access to user-defined mappings, and enables different
hardware remapping tricks like treating the spacebar as hyper when it's used
as a modifier."
  (dolist (key memes/keybindings-to-remap)
    (let ((s (if (integerp key) (char-to-string key) key)))
      (define-key input-decode-map
        (kbd (format "H-%s" s))
        (kbd (format "C-c %s" s))))))

;; Do the rebinding if memes/map-hyper-to-C-c is true
(when memes/keybindings-to-remap
  (memes/remap-hyper-to-C-c))

;; Keyboard and binding packages

(use-package general
  :ensure t)

(use-package free-keys
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :defer t
  :init (after-init #'which-key-mode)
  :config
  (validate-setq
   which-key-sort-order 'which-key-key-order-alpha
   which-key-side-window-max-width 0.4
   which-key-idle-delay 0.4))

(use-package keyfreq
  :ensure t
  :defer t
  :init
  :preface
  (defvar keyfreq-file (memes/var "keyfreq"))
  :init
  (progn
    (after-init #'keyfreq-mode)
    (after-init #'keyfreq-autosave-mode))
  :config
  (validate-setq keyfreq-excluded-commands
		 '(self-insert-command
		   outshine-self-insert-command
		   org-self-insert-command
		   abort-recursive-edit
		   forward-char
		   backward-char
		   previous-line
		   next-line)))

(use-package hydra
  :ensure t
  :defer t
  :config
  (use-package lv
    :config
    (validate-setq lv-use-separator t)))

(use-package interaction-log
  :ensure t
  :defer t
  :commands interaction-log-mode)

;; Buffer lists
(defhydra hydra-buffers (:color red)
  "Buffers"
  ("C-c b" memes/switch-to-last-buffer "last")
  ("TAB" memes/switch-to-last-buffer "last")
  ("`" memes/switch-to-last-buffer "last")
  ("h" bury-buffer "hide")
  ("k" kill-this-buffer "kill")
  ("K" kill-buffer-and-window "kill (window)")
  ("n" next-buffer "next")
  ("p" previous-buffer "previous")
  ("r" revert-buffer "revert"))

(hydra-set-property 'hydra-buffers :verbosity 1)

(general-define-key
 :prefix "C-c"
 :infix "b"
 "C-c b" '(memes/switch-to-last-buffer :which-key "last")
 "TAB" '(memes/switch-to-last-buffer :which-key "last")
 "h" '(hydra-buffers/bury-buffer :which-key "hide")
 "k" '(hydra-buffers/kill-this-buffer :which-key "kill")
 "K" '(hydra-buffers/kill-buffer-and-window :which-key "kill (window)")
 "n" '(hydra-buffers/next-buffer :which-key "next")
 "p" '(hydra-buffers/previous-buffer :which-key "previous")
 "r" '(hydra-buffers/revert-buffer :which-key "revert")
 "t" '(memes/temp-buffer :which-key "temp"))
 
;; Find stuff
(general-define-key
 :prefix "C-c"
 :infix "f"
 "C-c f" '(counsel-find-file :which-key "find-file")
 "f" '(counsel-find-file :which-key "find-file")
 "r" '(counsel-rg :which-key "ripgrep")
 "g" '(counsel-git-grep :which-key "git-grep")
 "p" '(projectile-find-file :which-key "(projectile) find-file")
 "t" '(memes/counsel-git-grep-project-todos :which-key "todos"))

;; Magit
(general-define-key
 :prefix "C-c"
 :infix "v"
 "C-c v" '(magit-status :which-key "magit")
 "B" '(magit-branch-popup :which-key ">branch")
 "d" '(magit-ediff-dwim :which-key "diff dwim")
 "D" '(magit-diff-popup :which-key ">diff")
 "f" '(magit-pull :which-key "pull")
 "C-c f" '(magit-pull-popup :which-key ">pull")
 "g" '(magit-status :which-key "magit")
 "l" '(magit-log-all :which-key "log")
 "C-c l" '(magit-log-popup :which-key ">log")
 "p" '(magit-push :which-key "push")
 "C-c p" '(magit-push-popup :which-key ">push")
 "s" '(magit-stage-file :which-key "stage")
 "t" '(git-timemachine :which-key "timemachine"))

;; Projectile
(general-define-key
 :prefix "C-c"
 :infix "p"
 "i" '(projectile-project-info :which-key "info")
 "f" '(projectile-find-file :which-key "find file")
 "r" '(projectile-recentf :which-key "recent")
 "z" '(projectile-cache-current-file :which-key "cache current file")
 "x" '(projectile-remove-known-project :which-key "remove known project")
 "d" '(projectile-find-dir :which-key "find directory")
 "b" '(projectile-switch-to-buffer :which-key "switch to buffer")
 "c" '(projectile-invalidate-cache :which-key "clear cache")
 "X" '(projectile-cleanup-known-projects :which-key "cleanup known projects")
 "o" '(projectile-multi-occur :which-key "multi occur")
 "s" '(projectile-switch-project :which-key "switch project")
 "k" '(projectile-kill-buffers :which-key "kill buffers"))

;; Global key bindings
(general-define-key
 :keymaps 'global
 "M-r" 'memes/revert-buffer-noconfirm
 "C-x C-r" 'counsel-recentf)

(provide 'config-keybindings)
;;; config-keybindings.el ends here
