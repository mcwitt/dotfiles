;;; Package --- Summary
;;; Commentary:
;;; Code:

(defvar git-sync-buffer-name "*async git-sync*")
(defvar git-sync-command "git-sync")

(defun git-sync-sentinel (process event)
  "Watches the git-sync PROCESS for an EVENT indicating a successful sync and closes the window."
  (message event)
  (cond ((string-match-p "finished" event)
         (message "git-sync successful")
         (kill-buffer git-sync-buffer-name))
        ((string-match-p "\\(exited\\|dumped\\)" event)
         (message "git-sync failed")
         (when (yes-or-no-p "Error running git-sync. Switch to output?")
           (switch-to-buffer git-sync-buffer-name)))))

(defun git-sync ()
  "Run git-sync as an async process."
  (interactive)
  (let* ((process (start-process-shell-command
                   "git-sync"
                   git-sync-buffer-name
                   git-sync-command)))
    (set-process-sentinel process 'git-sync-sentinel)))

(provide 'git-sync)
;;; git-sync.el ends here
