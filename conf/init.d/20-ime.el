;;; IME(SKK, sekka, mozc...)についての設定を記述する。

;; (@* "SKKについての設定")
;; SKK固有の設定については、~/.skkに設定を記述する
(require 'skk-setup)

(global-set-key [henkan] 'skk-mode)
(setq default-input-method "japanese-skk")
(setq skk-isearch-start-mode 'latin)
(setq skk-preload t)
