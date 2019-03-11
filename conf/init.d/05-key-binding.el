;;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'use-package))
;; グローバルなキーバインド/keybindに関連するpackageを設定する

;; Emacs標準機能関係
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-?") 'help-for-help)
(global-set-key (kbd "M-d") 'my:kill-word-at-point)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-x /") 'dabbrev-expand)
(global-set-key (kbd "C-x ,") 'delete-region)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-_") 'redo)

;; flymake関連
(global-set-key (kbd "C-c d") 'credmp/flymake-display-err-minibuf)

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

;; sdic
(global-set-key (kbd "C-c w") 'sdic-describe-word)

;; isearch
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(use-package which-key
  :ensure t
  :custom
  (which-key-use-C-h-commands t)
  :hook
  ((after-init . which-key-mode)))

(use-package hydra
  :ensure t)
