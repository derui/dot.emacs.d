;; はてな投稿関係の処理

;; (@* "hatena-diaryについての設定")
(autoload 'hatena:d:list "hatena-diary" "List Hatena::Diary blog entries in a buffer." t)
(autoload 'hatena:d:list-draft "hatena-diary" "List Hatena::Diary blog entries in a buffer." t)
(eval-after-load 'hatena-diary
  '(load (locate-user-emacs-file ".hatena-credentials.gpg")))
(require 'hatena-markup-mode)
(setq hatena:d:major-mode 'hatena:markup-mode)

;; articleとしてデータを設定する。
(defun my:hatena-prepend-article ()
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  (open-line 2)
  (insert (format-time-string "*%s*" (current-time))))
;; (define-key hatenahelper-mode-map (kbd "C-c C-i") 'my:hatena-prepend-article)

(defun my:hatena-mode-hook-0 ()
  (font-lock-fontify-buffer)
  (define-key hatena:markup-mode-map (kbd "C-c C-i") 'my:hatena-prepend-article)
  )
(add-hook 'hatena:markup-mode-hook 'my:hatena-mode-hook-0)
