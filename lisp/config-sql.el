;;; config-sql.el --- Configure generic SQL support

;;

;;; Commentary:

;;

;;; Code:
(require 'use-config)

(use-package sql
  :defer t
  :config
  (progn
    (after 'aggressive-indent
      (add-hook 'sql-mode-hook #'aggressive-indent-mode))
    (after 'paredit
      (add-hook 'sql-mode-hook #'paredit-mode))
    (validate-setq sql-send-terminator t)))

(use-package sql-indent
  :ensure t
  :defer t
  :init
  (progn
    (after 'sql
      (require 'sql-indent)))
  :config
  (setq sql-indent-offset 2
        sql-indent-first-column-regexp
        (concat "\\(^\\s-*"
                (regexp-opt '("with"
                              "window"
                              "inner"
                              "left"
                              "outer"
                              "right"
                              "select"
                              "update"
                              "insert"
                              "delete"
                              "union"
                              "intersect"
                              "from"
                              "where"
                              "into"
                              "group"
                              "having"
                              "order"
                              "set"
                              "create"
                              "drop"
                              "truncate"
                              "begin"
                              "end"
                              "lock"
                              "commit"
                              "alter"
                              "add"
                              "returning"
                              "copy"
                              "set"
                              "--"
                              "\\^L") t)
                "\\(\\b|\\s-\\)\\|\\(^```$\\)")))

(provide 'config-sql)
;;; config-sql.el ends here
