;; Settings common to all hosts to be executed last
;; $Id: 98common.el 1 2007-10-18 23:15:33Z memes $

;; Disable backtrace on error
(setq debug-on-error nil)

(if (memq window-system '(x win32 w32))
    (progn
      (define-key global-map [?\C-x ?\C-d] 'planner-diary-add-entry)
      (define-key global-map [?\C-x ?\C-n] 'remember)
      ))
