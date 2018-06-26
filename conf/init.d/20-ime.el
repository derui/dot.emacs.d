;;; IME(SKK, sekka, mozc...)についての設定を記述する。
(eval-when-compile
  (require 'use-package))

;; (@* "Mozcについての設定")
(when (boundp 'my:mozc-helper-locate)
  (defvar my:input-method "japanese-mozc")
  (require 'mozc)
  (setq mozc-candidate-style 'echo-area)
  (setq-default default-input-method my:input-method)
  (setq default-input-method my:input-method)
  (setq mozc-helper-program-name my:mozc-helper-locate)

  (use-package mozc-popup
    :ensure t
    :config
    ;; popup スタイルを使用
    (setq mozc-candidate-style 'popup))

  (defun my:disable-mozc ()
    (interactive)
    (set-input-method nil))

  (global-set-key (kbd "<Hangul>") #'my:enable-mozc)
  (global-set-key (kbd "<henkan>") #'my:enable-mozc)
  (global-set-key (kbd "<Hangul_Hanja>") #'my:disable-mozc)
  (global-set-key (kbd "<muhenkan>") #'my:disable-mozc))
