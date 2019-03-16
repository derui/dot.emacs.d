;;; IME(SKK, sekka, mozc...)についての設定を記述する。
(eval-when-compile
  (require 'use-package))

(defun my:disable-mozc ()
  (interactive)
  (set-input-method nil))

(defun my:enable-mozc ()
  (interactive)
  (set-input-method 'japanese-mozc))

;; (@* "Mozcについての設定")
(when (boundp 'my:mozc-helper-locate)
  (defvar my:input-method "japanese-mozc")
  (setq-default default-input-method my:input-method)
  (setq default-input-method my:input-method)

  (use-package mozc
    :custom
    (mozc-candidate-style 'echo-area)
    (mozc-helper-program-name my:mozc-helper-locate))

  (use-package mozc-popup
    :after (mozc)
    :custom
    ;; popup スタイルを使用
    (mozc-candidate-style 'popup))

  (global-set-key (kbd "<Hangul>") #'my:enable-mozc)
  (global-set-key (kbd "<henkan>") #'my:enable-mozc)
  (global-set-key (kbd "<Hangul_Hanja>") #'my:disable-mozc)
  (global-set-key (kbd "<muhenkan>") #'my:disable-mozc))
