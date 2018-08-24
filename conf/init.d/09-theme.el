(eval-when-compile
  (require 'use-package))

;; フレームについての設定。コンソールで実行された場合にはこの設定は無視される。
(when window-system

  ;; カーソルの点滅を止める。
  (if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

  ;; フレーム状態の初期設定を行う。
  (mapc (lambda (ls)
          (progn
            (add-to-list ls '(scroll-left . nil))
            (add-to-list ls '(background-mode . dark))))
        '(initial-frame-alist default-frame-alist))

  ;; show indicator for buffer bondaries.
  (setq-default indicate-buffer-boundaries
                '((top . nil) (bottom . right) (down . right)))

  (use-package dimmer
    :ensure t
    :hook ((focus-out-hook . my:dimmer-off)
           (focus-in-hook . my:dimmer-on))
    :config
    (setq differ-fraction 0.5)
    (setq dimmer-exclusion-regexp "^ \\*Minibuf")

    (defun my:dimmer-off ()
      (dimmer-mode -1)
      (dimmer-process-all))

    (defun my:dimmer-on ()
      (dimmer-mode 1)
      (dimmer-process-all))
    (dimmer-mode 1)))

(defvar my:var:current-theme 'gruvbox-dark-hard "current theme for me")
;; テーマについての設定
(load-theme my:var:current-theme t)
(enable-theme my:var:current-theme)
