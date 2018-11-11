;;; config-swift.el --- support Swift in Emacs

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package swift-mode
  :ensure t
  :defer t
  :init
  (progn
    (after 'flycheck
      (add-to-list 'flycheck-checkers 'swift)
      (setq flycheck-swift-target nil))
    (setq swift-repl-executable (cond (memes/is-mac "xcrun swift")
                                      (t "swift")))))

(provide 'config-swift)
;;; config-swift.el ends here
