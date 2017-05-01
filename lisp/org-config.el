;;; org-config.el --- configuration for org-mode

;;; Commentary:

;;; Code:

;; Prefer OS package for org-mode - only enable if installed
(cond ((fboundp 'org-mode)
       ;; Integrate org-mode
       (global-set-key "\C-cl" 'org-store-link)
       (global-set-key "\C-ca" 'org-agenda)
       (global-set-key "\C-cb" 'org-iswitchb)
       (setq-default org-insert-mode-line-in-empty t
		     org-plantuml-jar-path (convert-standard-filename (expand-file-name "lib/plantuml.jar" user-emacs-directory)))
       ))

(provide 'org-config)
;;; org-config.el ends here
