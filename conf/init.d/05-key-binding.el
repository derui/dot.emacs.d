(require 'use-package)
;; グローバルなキーバインドを登録する。
;; グローバルであれば、外部elispのものであってもここに設定する。
;; 外部elispのものである場合はまとめて設定しておくこと。

;; Emacs標準機能関係
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-?") 'help-for-help)
(global-set-key (kbd "M-d") 'kill-word-at-point)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-x /") 'dabbrev-expand)
(global-set-key (kbd "C-x ,") 'delete-region)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-_") 'redo)

;; flymake関連
(global-set-key (kbd "C-c d") 'credmp/flymake-display-err-minibuf)

;; ユーザーが作成した機能についてのバインディング
(global-set-key [f2] 'my:swap-screen)
(global-set-key [S-f2] 'my:swap-screen-with-cursor)

;; マウスのホイールスクロールスピードを調節
;; (連続して回しているととんでもない早さになってしまう。特にLogicoolのマウス)
(global-set-key [wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [double-wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [double-wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [triple-wheel-up] '(lambda () "" (interactive) (scroll-down 2)))
(global-set-key [triple-wheel-down] '(lambda () "" (interactive) (scroll-up 2)))

;; org-mode関係
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c")  'org-capture)
(global-set-key (kbd "C-x C-r") 'org-remember-code-reading)

;; git-dwim
(global-set-key (kbd "C-x v B") 'git-branch-next-action)

;; smartrepは、keymapが登録されていることが前提になっているので、ここで設定しておく
;; C-q prefixは空いているので, '{'じゃなくて '['にして Shiftも節約
(let (keymap)


  (use-package smartrep
    :init
    (progn
      (setq keymap '(("[" . (backward-paragraph))
                     ("]" . (forward-paragraph))))
      (add-to-list 'keymap `("M-p" . 'mc/mark-previous-like-this))
      (add-to-list 'keymap `("M-n" . 'mc/mark-next-like-this)))
    :config
    (progn
      (smartrep-define-key global-map "C-q" keymap)
      (smartrep-define-key global-map "M-g"
        '(("n"   . 'next-error)
          ("p"   . 'previous-error)
          ("C-n" . 'next-error)
          ("C-p" . 'previous-error)))
      )
    )

  )

;; sdic
(global-set-key (kbd "C-c w") 'sdic-describe-word)

;; isearch
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
