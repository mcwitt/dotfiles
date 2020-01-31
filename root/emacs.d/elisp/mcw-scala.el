;;; package --- Personal configuration for Scala development
;;; Commentary:
;;; Code:
(use-package scala-mode
  :after smartparens
  :hook ((scala-mode . smartparens-mode)
	 (scala-mode . fira-code-mode))
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package ensime
  :hook scala-mode
  :bind (:map scala-mode-map
	 ("C-c C-e" . ensime)
	 ("C-c C-f" . mcw:scala-mode-format-buffer-with-sbt-scalafmt)
	 :map ensime-mode-map
	 ("C-c C-v g" . ensime-edit-definition-of-thing-at-point))
  :custom (ensime-startup-notification 'nil))

(defun mcw:scala-mode-format-buffer-with-sbt-scalafmt ()
  (interactive)
  (save-buffer)
  (ensime-sbt-run-command-in-project "scalafmtOnly" t)
  (revert-buffer :ignore-auto :noconfirm))

(provide 'mcw-scala)
;;; mcw-scala.el ends here
