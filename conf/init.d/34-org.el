(eval-when-compile
  (require 'use-package))

(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  ;; org-mode内部のソースを色付けする
  (setq org-src-fontify-natively t)

  (add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only

  ;; 一時間に一回、org-modeの全てのバッファを保存する。
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)

  ;; org-modeの開始時に、行の折り返しを無効にする。
  (setq org-startup-truncated t)
  ;; follow-linkから戻ることを可能とする。
  (setq org-return-follows-link t))

(use-package log4e)
(use-package org-pomodoro :defer t
  :bind (:map org-mode-map
              ("C-c m" . org-pomodoro)))
