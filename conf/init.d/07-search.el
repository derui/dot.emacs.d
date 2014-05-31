
;; (@* "grepされた結果に対して編集を行うgrep-edit")
(require 'grep-edit)

;; grep-editの開始準備を行う
(defun my:grep-edit-setup ()
  (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map (kbd "C-c C-c") 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  )
(add-hook 'grep-setup-hook 'my:grep-edit-setup t)
