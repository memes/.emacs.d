;;; config-browser.el --- integration with w3m

;;; Commentary:

;;; Code:
(require 'use-config)

;; eww for browsing in Emacs
(use-package eww
  :ensure t
  :defer t
  :commands eww eww-follow-link
  :init
  (after (config-path config-platform)
    (setq browse-url-browser-function '(("." . eww-browse-url))
          eww-download-directory "~/Downloads"
          eww-search-prefix "https://www.google.com/search?q="
          url-configuration-directory (memes/var "url")
          browse-url-generic-program (executable-find (cond (memes/is-linux "xdg-open")
                                                            (memes/is-mac "open")
                                                            (t "chrome")))))
  :hook
  ((eww-mode . toggle-word-wrap)
   (eww-mode . visual-line-mode)))

(provide 'config-browser)
;;; config-browser.el ends here
