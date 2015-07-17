(require 'scala-mode2)

(add-to-list 'load-path (locate-user-emacs-file
                         (concat "/site-lisp/ensime/elisp")))
(require 'ensime)

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(defadvice scala-block-indentation (around improve-indentation-after-brace activate)
  (if (eq (char-before) ?\{)
      (setq ad-return-value (+ (current-indentation) scala-mode-indent:step))
    ad-do-it))

(defun my:scala-mode-1 ()
  (auto-complete-mode -1)
  )

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook #'my:scala-mode-1)
