;;; config-dart.el --- Dart language support in Emacs

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package dart-mode
  :ensure t
  :defer t
  :config
  (validate-setq dart-enable-analysis-server t
                 dart-format-on-save t)
  :hook
  (dart-mode . flycheck-mode))

(provide 'config-dart)
;;; config-dart.el ends here
