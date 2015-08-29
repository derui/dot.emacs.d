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

(defun my:scala-mode-hook-1 ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-ensime-completions ac-source-words-in-same-mode-buffers))
  (make-local-variable 'ac-expand-on-auto-complete)
  (setq ac-expand-on-auto-complete t)
  ;; あいまい検索だと多すぎるので、曖昧にしないように
  (make-local-variable 'ac-use-fuzzy)
  (setq ac-use-fuzzy nil)
  ;; 自動では開始しないように
  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)
  ;; 常にメニューを表示するように
  (make-local-variable 'ac-auto-show-menu)
  (setq ac-auto-show-menu t)
  (make-local-variable 'ac-trigger-key)
  (setq ac-trigger-key "TAB")
  )

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'my:scala-mode-hook-1)

(custom-set-variables
 '(ensime-ac-override-settings nil)
 '(ensime-completion-style 'auto-complete)
 '(ensime-auto-generate-config t)
 )
