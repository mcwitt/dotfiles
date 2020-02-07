;;; package --- Personal configuration for SQL connections
;;; Commentary:
;;; Code:

(defun mcw:sql-connect ()
  (interactive)
  (if (not (boundp 'sql-connections)) (mcw:load-sql-connections) nil)
  (call-interactively 'sql-connect))
(defun mcw:load-sql-connections ()
  (interactive)
  (require 'sql-connections "~/.sql-connections.el.gpg"))

(use-package sql
  :hook (sql-interactive-mode . (lambda () (toggle-truncate-lines t)))
  :bind ("C-c s" . mcw:sql-connect)
  :commands sql-connect)

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(provide 'mcw-sql)
;;; mcw-sql.el ends here
