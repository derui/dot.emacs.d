;; sdicの内容を現在の箇所に表示するelisp
;; (auto-install-from-emacswiki "pos-tip.el")
;; (auto-install-from-emacswiki "sdic-inline-pos-tip.el")
(el-get 'sync '(pos-tip))
(require 'pos-tip)
(require 'sdic-inline)
(require 'sdic-inline-pos-tip)

(setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
(setq sdic-inline-display-func 'sdic-inline-pos-tip-show)

;; 文字色
(setq sdic-face-color "pink")
