;;; -*- lexical-binding: t -*-

;; configuration for OS specific

(when window-system
  (cond
   ((and (string-equal window-system "w32"))
    ;; ここでは、とりあえずwindow7以降で利用することを想定して、
    ;; メイリオを利用する。
    ;; メイリオ系のフォントの中でも、特にCUI的な利用に特化して作成された
    ;; MeiryoKe_Consoleを利用する。
    (let (window-fontname)
      (setq window-fontname
            (create-fontset-from-ascii-font
             "-outline-MeiryoKe_Console-normal-r-normal-normal-16-*-*-*-*-*-iso8859-1"
             nil "meiryo"))

      (set-fontset-font "fontset-meiryo"
                        'japanese-jisx0208
                        '("MeiryoKe_Console" . "unicode-bmp"))
      (set-fontset-font "fontset-meiryo"
                        'katakana-jisx0201
                        '("MeiryoKe_Console" . "unicode-bmp"))

      (set-frame-font window-fontname)))

   ((eq window-system 'x)
    ;; window-systemがxの場合に実行される初期化elisp
    ;; x11を利用している場合、クリップボードの連携を有効にする。
    (setq select-enable-clipboard t
          select-enable-primary t))))
