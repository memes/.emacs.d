;;; config-search.el --- Configure searching in Emacs

;;

;;; Commentary:
;;

;;; Code:
(require 'use-config)

(use-package isearch
  :defer t
  :commands (isearch-forward-symbol-at-point isearch-forward)
  :init
  (progn
    ;; Reveal content of subtrees during isearch, also see reveal-mode
    (setq-default isearch-invisible 'open)
    (validate-setq isearch-allow-scroll t
                   lazy-highlight-initial-delay 0))
  :config
  (bind-keys
   :map isearch-mode-map
   ;; Allow deleting chars in the search string, use C-r to search backwards
   ([remap isearch-delete-char] . isearch-del-char)))

(use-package anzu
  :ensure t
  :init
  (global-anzu-mode))

(use-package imenu-anywhere
  :ensure t
  :defer t)

(use-package link-hint
  :ensure t
  :defer t)

(use-package goto-last-change
  :ensure t
  :defer t
  :commands goto-last-change)

(use-package swiper
  :ensure t
  :defer t
  :config
  (advice-add 'swiper :after #'recenter-top-bottom))

(use-package avy
  :ensure t
  :config
  (progn
    (validate-setq avy-style 'at-full
                   avy-background t
                   avy-all-windows t
                   avy-timeout-seconds 0.3)
    ;; Use C-' in isearch to bring up avy
    (avy-setup-default)))

;;;###autoload
(defun memes/swiper-at-point (_arg)
  "Swiper with 'thing-at-point'."
  (interactive "P")
  (swiper (thing-at-point 'symbol)))

(provide 'config-search)
;;; config-search.el ends here
