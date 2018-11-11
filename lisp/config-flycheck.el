;;; config-flycheck.el --- Configures flycheck for all programing modes

;;

;;; Commentary:

;;

;;; Code
(require 'use-config)

;; Flycheck - common settings
(use-package flycheck
  :ensure t
  :defer t
  :init (after-init #'global-flycheck-mode)
  :commands (flycheck-mode flycheck-list-errors)
  :defines
  (flycheck-error-list-buffer
   flycheck-display-errors-function)
  :functions
  (flycheck-buffer
   flycheck-list-errors
   flycheck-display-error-messages
   flycheck-error-list-mode
   flycheck-error-pos
   flycheck-error-list-set-source
   flycheck-error-list-reset-filter
   memes/flycheck-turn-messages-on
   memes/flycheck-turn-messages-off)
  :config
  (progn
    ;; Custom
    (validate-setq flycheck-emacs-lisp-load-path 'inherit)
    ;; Advices
    (defun memes/flycheck-select-window ()
      (select-window (get-buffer-window flycheck-error-list-buffer)))
    (advice-add #'flycheck-list-errors :after #'memes/flycheck-select-window)
    ;; Conditionally disabled error messages
    (defvar memes/flycheck-display-errors-function #'flycheck-display-error-messages
      "Defines the function to use for displaying flycheck errors.")
    (defun memes/flycheck-turn-messages-off (&optional _)
      "Prevent flycheck from displaying messages."
      (setq flycheck-display-errors-function nil))
    (defun memes/flycheck-turn-messages-on (&optional _)
      "Enable display of flycheck messages."
      (setq flycheck-display-errors-function memes/flycheck-display-errors-function)
      (flycheck-buffer))
    (defun memes/flycheck-edebug-toggle ()
      "Enable/disable flycheck messages if edebug-mode is inactive/active."
      (if (bound-and-true-p edebug-mode)
          (memes/flycheck-turn-messages-off)
        (memes/flycheck-turn-messages-on)))
    ;; Edebug prints to the echo area as well
    (after 'edebug
      (add-hook 'edebug-mode-hook #'memes/flycheck-edebug-toggle))
    ;; Allow company to display documentation in the modeline
    (after 'company
      (add-hook 'company-completion-started-hook #'memes/flycheck-turn-messages-off)
      (add-hook 'company-completion-finished-hook #'memes/flycheck-turn-messages-on)
      (add-hook 'company-completion-cancelled-hook #'memes/flycheck-turn-messages-on))
    (after 'counsel
      ;; https://github.com/nathankot/dotemacs/blob/ef76773c69cac36c04935edcfa631052bd2c679d/init.el#L566
      (defvar memes/counsel-flycheck-history nil
        "History for `counsel-flycheck`.")
      (defun memes/counsel-flycheck ()
        (interactive)
        (if (not (bound-and-true-p flycheck-mode))
            (message "Flycheck mode is not available or enabled.")
          (ivy-read "Flycheck: "
                    (let ((source-buffer (current-buffer)))
                      (with-current-buffer (or (get-buffer flycheck-error-list-buffer)
                                               (progn
                                                 (with-current-buffer
                                                     (get-buffer-create flycheck-error-list-buffer)
                                                   (flycheck-error-list-mode)
                                                   (current-buffer))))
                        (flycheck-error-list-set-source source-buffer)
                        (flycheck-error-list-reset-filter)
                        (revert-buffer t t t)
                        (split-string (buffer-string) "\n" t " *")))
                    :action (lambda (s &rest _)
                              (-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
                                            (pos (flycheck-error-pos error)) )
                                (goto-char (flycheck-error-pos error))))
                    :history 'memes/counsel-flycheck-history))))))

(provide 'config-flycheck)
;; config-flycheck.el ends here
