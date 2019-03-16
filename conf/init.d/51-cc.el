(eval-when-compile
  (require 'use-package))

;; (@* "C++-modeについての設定")
;; .hはc++-modeで開く
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)) auto-mode-alist))

(defun my:c-mode-hook ()
  (setq completion-mode t)
  ;; compile-windowの設定
  (setq compilation-buffer-name "*compilation*")
  (setq compilation-scroll-output t)
  (setq compilation-read-command t)
  (setq compilation-ask-about-save nil)
  (setq compilation-window-height 10)
  (setq compile-command "make")
  ;; cc-mode内で定義されるキーバインド
  (define-key c-mode-base-map (kbd "C-c C-c")   'comment-region)
  (define-key c-mode-base-map (kbd "C-c C") 'my-c++-cast)
  (define-key c-mode-base-map (kbd "C-c C-M-c") 'uncomment-region)
  (define-key c-mode-base-map (kbd "C-c e")      'c-macro-expand)
  (define-key c-mode-base-map (kbd "C-c c")      'my-compile)
  (define-key c-mode-base-map (kbd "C-c M-c")   'compilation-close)
  (define-key c-mode-base-map (kbd "C-c g")      'gdb)
  (define-key c-mode-base-map (kbd "C-c t")      'toggle-source)
  (define-key c-mode-base-map (kbd "C-c C-d") 'c-down-conditional)
  ;; cc-modeに入る時に自動的にgtags-modeにする
  (gtags-mode t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cc-modeに入るときに呼び出す hook の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          #'my:c-mode-hook)

;; googleのコーティング規約に依存するための設定
(use-package google-c-style
  :hook ((c-mode-common-hook . google-set-c-style)
         (c-mode-common-hook . google-make-newline-indent)))
