;; フレームについての設定。コンソールで実行された場合にはこの設定は無視される。
(when window-system

  ;; カーソルの点滅を止める。
  (if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

  ;; フレーム状態の初期設定を行う。
  (setq initial-frame-alist
        (add-to-list
         'initial-frame-alist
         '(scroll-left . nil)
         ))
  )
