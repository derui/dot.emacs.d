;; フレームについての設定。コンソールで実行された場合にはこの設定は無視される。
(when window-system

  ;; カーソルの点滅を止める。
  (if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

  ;; 保存されているフレーム状態保存ファイルがあればそれを読み出し、存在しない場合は
  ;; 初期設定を行う。
  ;; フレーム状態の初期設定を行う。
  (setq initial-frame-alist
        (append
         (list
          '(background-color . "gray14")
          '(foreground-color . "white")
          '(cursor-color . "Gray")
          '(scroll-left . nil)
          initial-frame-alist)))
  (set-foreground-color "White")
  (set-cursor-color "Gray")
  )

