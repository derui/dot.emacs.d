;; 各種バッファーに関連する設定

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("\*omake\*.*" :noselect t :regexp t))

