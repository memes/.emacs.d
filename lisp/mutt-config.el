;; Configuration options for mutt integration
;; $Id: mutt-config.el 6 2008-06-09 15:59:57Z memes $

(autoload 'muttrc-mode "muttrc-mode.el"
  "Major mode to edit muttrc files" t)
(setq auto-mode-alist
      (append '(("muttrc\\'" . muttrc-mode)) auto-mode-alist))

(defadvice gnuserv-process-filter (after post-mode-message first activate)
  "If the buffer is in post-mode, override the gnuserv-edit message with a post-save-current-buffer-and-exit message."
  (if (eq major-mode 'post-mode)
      (message
       (substitute-command-keys "Type \\[describe-mode]] for help composing; \\[post-save-current-buffer-and-exit] when done."))))

(add-hook 'gnuserv-visit-hook
	  (function (lambda()
		      (cond ((string-match "Post" mode-name)
			     (post-goto-body))))))
(setq post-should-prompt-for-attachment 'Never);

(defun memes-post-mode-hook ()
  (turn-on-auto-fill)
  (turn-on-font-lock)
  (set-buffer-modified-p nil)
  (post-goto-body)
  (beginning-of-line)
  )
(add-hook 'post-mode-hook 'memes-post-mode-hook)
(setq auto-mode-alist
      (cons (cons (concat "mutt-" my-hostname "-[0-9]+-[0-9]+") 'post-mode)
	auto-mode-alist))