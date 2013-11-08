;; グローバルなキーバインドを登録する。
;; グローバルであれば、外部elispのものであってもここに設定する。
;; 外部elispのものである場合はまとめて設定しておくこと。

;; Emacs標準機能関係
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-?") 'help-for-help)
(global-set-key (kbd "M-d") 'kill-word-at-point)
(global-set-key (kbd "C-o") 'my:search-forward-with-char)
(global-set-key (kbd "C-S-o") 'my:search-backward-with-char)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-x /") 'dabbrev-expand)
(global-set-key (kbd "C-x ,") 'delete-region)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-_") 'redo)

;; flymake関連
(global-set-key (kbd "C-c d") 'credmp/flymake-display-err-minibuf)

;; ウィンドウ移動関連については、c-qをプレフィックスとする。
(defvar my:ctrl-q-prefix-map (make-sparse-keymap))
(global-set-key (kbd "C-q") my:ctrl-q-prefix-map)
(define-key my:ctrl-q-prefix-map (kbd "C-v") 'quoted-insert)
(define-key my:ctrl-q-prefix-map (kbd "C-r") 'my:window-resizer)

;;; C-q hjklで上下左右にウィンドウを移動する。
(define-key my:ctrl-q-prefix-map (kbd "l") 'windmove-right)
(define-key my:ctrl-q-prefix-map (kbd "h") 'windmove-left)
(define-key my:ctrl-q-prefix-map (kbd "j") 'windmove-down)
(define-key my:ctrl-q-prefix-map (kbd "k") 'windmove-up)
;;; C-q dでウィンドウを削除する
(define-key my:ctrl-q-prefix-map (kbd "d") 'delete-window)
(define-key my:ctrl-q-prefix-map (kbd "C-q") 'my:other-window)

(define-key my:ctrl-q-prefix-map (kbd "s") 'split-window-vertically)
(define-key my:ctrl-q-prefix-map (kbd "v") 'split-window-horizontally)
;;; C-qへの独自キーマップ設定 ここまで

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
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c")  'org-capture)
(global-set-key (kbd "C-x C-r") 'org-remember-code-reading)

;; helm関係
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-M-s") 'helm-occur)
(global-set-key (kbd "C-c C-f") 'helm-project) ;; open-junk-file
(global-set-key (kbd "C-c C-j") 'open-junk-file)

;; git-dwim
(global-set-key (kbd "C-x v B") 'git-branch-next-action)

;; smartrepは、keymapが登録されていることが前提になっているので、ここで設定しておく
;; C-q prefixは空いているので, '{'じゃなくて '['にして Shiftも節約
(let (keymap)

  (setq keymap '(("[" . (backward-paragraph))
                 ("]" . (forward-paragraph))))
  (add-to-list 'keymap `("M-p" . 'mc/mark-previous-like-this))
  (add-to-list 'keymap `("M-n" . 'mc/mark-next-like-this))
  (smartrep-define-key
      global-map "C-q" keymap)

  (smartrep-define-key global-map "M-g"
    '(("n"   . 'next-error)
      ("p"   . 'previous-error)
      ("C-n" . 'next-error)
      ("C-p" . 'previous-error)))
  )

;; id-manager
(global-set-key (kbd "M-7") 'id-manager)

;; expand-region
(global-set-key (kbd "C-@") 'er/expand-region)

;; ace-jump-mode
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; sdic
(global-set-key (kbd "C-c w") 'sdic-describe-word)

;; isearch
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
