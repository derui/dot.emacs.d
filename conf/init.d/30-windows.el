;; (@> "windows.elを利用して、ウィンドウ状態の補完を行う")
(setq win:switch-prefix (kbd "C-z"))
(define-key global-map win:switch-prefix nil)
(setq win:base-key ?`)
(setq win:max-configs 27)
(define-key ctl-x-map "C" 'save-buffers-kill-emacs) ;; C-x Cで保存せずに終了する。
(setq win:quick-selection nil) ;; C-c 英字にウィンドウを割り当てない。
(require 'windows)
;; フレームを使用しない。
(setq win:use-frame nil)
(win:startup-with-window)
(setq revive:ignore-buffer-pattern " *")  ;; 保存しないバッファの正規表現
