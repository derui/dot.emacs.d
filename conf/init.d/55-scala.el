(eval-when-compile
  (require 'use-package))

(use-package ensime
  :mode ("\\.scala$" . scala-mode)
  :hook (scala-mode-hook . ensime-scala-mode-hook)
  :config

  ;; This step causes the ensime-mode to be started whenever
  ;; scala-mode is started for a buffer. You may have to customize this step
  ;; if you're not using the standard scala mode.
  (defadvice scala-block-indentation (around improve-indentation-after-brace activate)
    (if (eq (char-before) ?\{)
        (setq ad-return-value (+ (current-indentation) scala-mode-indent:step))
      ad-do-it)))
