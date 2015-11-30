(require 'org)

;; org-modeを利用するための設定
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'org-mode-hook 'turn-on-font-lock) ; Org buffers only

;; 一時間に一回、org-modeの全てのバッファを保存する。
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; org-modeの開始時に、行の折り返しを無効にする。
(setq org-startup-truncated t)
;; follow-linkから戻ることを可能とする。
(setq org-return-follows-link t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-capture-templates (quote (("N" "Idea and memos" entry (file+headline "~/Dropbox/git/org/memo.org" "New Idea") "")
                                 ("D" "Daily work" entry (file+headline "~/Dropbox/git/org/working-clocks.org" "Works") "** %<%Y-%m-%d>")))))

(require 'log4e)
(require 'org-pomodoro)

(define-key org-mode-map (kbd "C-c m") #'org-pomodoro)
