;;; git-now.el - Call "git now" command

(provide 'git-now)

(defun now ()
  (interactive)
  (call-process-shell-command "git now" nil "*git now*" nil)
  (pop-to-buffer "*git now*" t nil)
  (other-window -1)
  (message "git now!"))
