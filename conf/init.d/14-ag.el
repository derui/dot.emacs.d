(require 'ag)

(custom-set-variables
 '(ag-higglight-search t)               ; 検索結果の中の検索語をハイライトする
 '(ag-reuse-window 'nil)                ; 現在のウィンドウを検索結果表示に使う
 '(ag-reuse-buffers 'nil))              ; 現在のバッファを検索結果表示に使う

(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)
