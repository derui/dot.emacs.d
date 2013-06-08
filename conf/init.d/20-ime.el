;;; IME(SKK, sekka, mozc...)についての設定を記述する。

;; (@* "SKKについての設定")


(defvar skk-user-directory nil)
(defvar skk-isearch-start-mode nil)
;; SKK固有の設定については、~/.emacs.d/etc/skkに設定を記述する
(setq skk-user-directory (concat user-emacs-directory "/etc/skk"))
;; migemoを利用するため、isearchの際には、ddskkには働かないでいてもらう
(setq skk-isearch-start-mode 'latin)

(require 'skk-setup)

(global-set-key [henkan] 'skk-mode)
(setq default-input-method "japanese-skk")
(setq skk-preload t)
