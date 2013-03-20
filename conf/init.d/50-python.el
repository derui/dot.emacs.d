;; (@* "python編集環境についての設定")
(autoload 'python-mode "python-mode" nil t)

(el-get 'sync '(python-mode))
;; 拡張子が.pyのものについて、python-modeを割り当てる
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; python-modeについて、インタープリタ名をpythonとする。
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; python-mode実行時に実行するhookの設定
(defun my:python-mode-hook-0 ()
  (setq indent-tabs-mode nil)
  (define-key py-mode-map "\C-h" 'py-electric-backspace)
  )
(add-hook 'python-mode-hook 'my:python-mode-hook-0)
