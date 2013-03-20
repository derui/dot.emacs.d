(el-get 'sync '(scala-mode
                ensime))

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(defadvice scala-block-indentation (around improve-indentation-after-brace activate)
  (if (eq (char-before) ?\{)
      (setq ad-return-value (+ (current-indentation) scala-mode-indent:step))
    ad-do-it))

(defun scala-newline-and-indent ()
  (interactive)
  (delete-horizontal-space)
  (let ((last-command nil))
    (newline-and-indent))
  (when (scala-in-multi-line-comment-p)
    (insert "* ")))

(add-hook 'scala-mode-hook
          (lambda ()
            (define-key scala-mode-map (kbd "RET") 'scala-newline-and-indent)))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'scala-electric-mode)
