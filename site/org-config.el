;; Configuration options for org-mode
;; $Id: $

(cond ((fboundp 'org-mode)
       ;; Integrate org-mode
       (global-set-key "\C-cl" 'org-store-link)
       (global-set-key "\C-ca" 'org-agenda)
       (global-set-key "\C-cb" 'org-iswitchb))
       (setq org-insert-mode-line-in-empty t)
      )

;; evernote integration
(require 'evernote-mode)
(setq evernote-username "memes"
      evernote-password-cache t
      evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
