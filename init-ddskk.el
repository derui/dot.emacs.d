;; (setq skk-sticky-key ";")
;; azikを利用するように
(setq skk-use-azik t)
(setq skk-azik-keyboard-type 'us101)
(add-hook 'skk-azik-load-hook
          (lambda()
            ;; azikから追加された各種拡張を、SKK寄りに戻すための追加設定
            ;; 「ん」をqに割り当てるのは、ただでさえ負荷の高い左小指を酷使することになるので、元に戻す
            ;; qの役割を元に戻したので、「も元に戻す
            (setq skk-rom-kana-rule-list
                  (-filter (lambda (v) (not (or (string= "q" (car v))
                                                (string= "[" (car v)))))
                           skk-rom-kana-rule-list))

            ;; Xで辞書登録する場合があるので、この場合でもちゃんと破棄できるようにする
            (setq skk-rom-kana-rule-list
                  (append skk-rom-kana-rule-list
                          '(("!" nil skk-purge-from-jisyo)
                            ("q" nil skk-toggle-characters)
                            ("[" nil "「"))
                          ))))
(require 'skk-azik)

;; 送り仮名が厳密に正しいものを優先して表示するようにする
(setq skk-henkan-strict-okuri-precedence t)
;; 変換モードでEnterを押しても改行しないように
(setq skk-egg-like-newline t)
;; 個人辞書の文字コード
(setq skk-jisyo-code 'utf-8-unix)
;; 辞書登録の際にミスを確認する
(setq skk-check-okurigana-on-touroku t)

(cond (my:use-skkserver
       (setq skk-server-host "localhost"
             skk-server-portnum "1178"
             skk-large-jisyo nil)
       (defun skk-open-server-decoding-utf-8 ()
         "辞書サーバと接続する。サーバープロセスを返す。 decoding coding-system が euc ではなく utf8 となる。"
         (unless (skk-server-live-p)
           (setq skkserv-process (skk-open-server-1))
           (when (skk-server-live-p)
             (let ((code (cdr (assoc "euc" skk-coding-system-alist))))
               (set-process-coding-system skkserv-process 'utf-8 code))))
         skkserv-process)
       (setq skk-mode-hook
             '(lambda()
                (advice-add 'skk-open-server :override 'skk-open-server-decoding-utf-8))))
      (t
       ;; 辞書を取得する。ただし、ここで取得する辞書はEUC-JPであり、このままだと個人辞書とのcoding違いで読み込めない。
       ;; そのため、取得した辞書に対して変換をかける必要がある。
       (setq skk-get-jisyo-directory (expand-file-name "skk-jisyo" user-emacs-directory)
             skk-large-jisyo (expand-file-name "SKK-JISYO.L" skk-get-jisyo-directory))))
