;; キーの連打感覚は0.2秒
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)

;; key-chordを有効にする
(key-chord-mode 1)
(key-chord-define-global "df" 'view-mode)
(key-chord-define-global "cv" 'describe-bindings)
