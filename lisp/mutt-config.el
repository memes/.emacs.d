;;; mutt-config.el --- configuration when using Mutt

;;; Commentary:

;;; Code:

(defvar memes-hostname)

;; Prefer OS package for mutt and post modes; only enable if found
(cond ((fboundp 'muttrc-mode)
       ;; Allow autoload of muttrc files
       (autoload 'muttrc-mode "muttrc-mode.el"
	 "Major mode to edit muttrc files" t)
       (setq auto-mode-alist
	     (append '(("muttrc\\'" . muttrc-mode)) auto-mode-alist))
       ))

;; When using gnuserv with post-mode, show status message appropriate for
;; mutt/post-mode integration
(defadvice gnuserv-process-filter (after post-mode-message first activate)
  "If the buffer is in post-mode, override the gnuserv-edit message with a post-save-current-buffer-and-exit message."
  (if (eq major-mode 'post-mode)
      (message
       (substitute-command-keys "Type \\[describe-mode]] for help composing; \\[post-save-current-buffer-and-exit] when done."))))

(add-hook 'gnuserv-visit-hook
	  (function (lambda()
		      (cond ((string-match "Post" mode-name)
			     (post-goto-body))))))

;; Hook to setup post mode buffer
(defun memes-post-mode-hook ()
  "Hook for post-mode."
  (turn-on-auto-fill)
  (turn-on-font-lock)
  (set-buffer-modified-p nil)
  (post-goto-body)
  (beginning-of-line))

;; Only enable post mode if installed as OS package
(cond ((fboundp 'post-mode)
       (add-hook 'post-mode-hook 'memes-post-mode-hook)
       (setq auto-mode-alist
	     ;; Recognise mutt temp file as a post-mode candidate
	     (cons (cons (concat "mutt-" memes-hostname "-[0-9]+-[0-9]+") 'post-mode)
		   auto-mode-alist))
       (setq-default post-should-prompt-for-attachment 'Never)))

(provide 'mutt-config)
;;; mutt-config.el ends here
