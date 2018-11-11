;;; config-elixir.el --- configure elixir support in Emacs

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t
  :hook
  (elixir-mode . alchemist-mode))

(provide 'config-elixir)
;;; config-elixir.el ends here
