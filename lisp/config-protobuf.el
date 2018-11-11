;;; config-protobuf.el --- Protobuffer editing support in Emacs

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package protobuf-mode
  :ensure t
  :defer t
  :init
  (progn
    (after 'flycheck
      (add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t))))

(provide 'config-protobuf)
;;; config-protobuf.el
