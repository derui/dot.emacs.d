;; モードラインに関係するパッケージの設定

;; (@* "同一名称のバッファの場合に、バッファの前にディレクトリ名を付与する")
(req uniquify
     (setq uniquify-buffer-name-style 'post-forward-angle-brackets))
