;;; package --- Miscellaneous elisp functions
;;; Commentary:
;;; Code:

(defun mcw:increment-number-at-point ()
  "Increment integer at the cursor position."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") #'mcw:increment-number-at-point)

(provide 'mcw-misc)
;;; mcw-misc.el ends here
