;; Configuration options for org-mode
;; $Id: $

(cond ((fboundp 'org-mode)
       ;; Integrate org-mode
       (global-set-key "\C-cl" 'org-store-link)
       (global-set-key "\C-ca" 'org-agenda)
       (global-set-key "\C-cb" 'org-iswitchb))
       (setq org-insert-mode-line-in-empty t)
       (setq org-plantuml-jar-path (convert-standard-filename (expand-file-name "lib/plantuml.jar" user-emacs-directory)))
      )
