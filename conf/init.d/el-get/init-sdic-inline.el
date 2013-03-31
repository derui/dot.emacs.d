;; sdicをインラインで表示する
(require 'sdic-inline)
(sdic-inline-mode 0)   ; sdic-inline モードの起動
;; 辞書ファイルの設定
(setq sdic-inline-eiwa-dictionary
      (concat user-emacs-directory "etc/dict/gene.sdic"))
(setq sdic-inline-waei-dictionary
      (concat user-emacs-directory "etc/dict/jedict.sdic"))

(setq sdic-inline-not-search-style 'point) ; デフォルト値。ポイント位置が前回と同じである限り、再度辞書ではひかない。
(add-to-list 'sdic-inline-enable-modes 'w3m-mode)  ; w3m-mode でも動作するようにする。
