(eval-when-compile
  (require 'use-package))

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :hook ((org-mode-hook . turn-on-font-lock)
         (kill-emacs . my:org-clock-out-and-save-when-exit))
  :config
  ;; org-mode内部のソースを色付けする
  (setq org-src-fontify-natively t)

  ;; 一時間に一回、org-modeの全てのバッファを保存する。
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)

  ;; org-modeの開始時に、行の折り返しを無効にする。
  (setq org-startup-truncated t)
  ;; follow-linkから戻ることを可能とする。
  (setq org-return-follows-link t)
  ;; org-clockのタイトルをディスプレイ
  (setq org-clock-clocked-in-display 'frame-title)
  (setq org-clock-out-remove-zero-time-clocks t)

  (defun my:org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))

  (defun my:org-agenda-skip-all-sibling-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (my:org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (my:org-current-is-todo)
            (setq should-skip-entry t)))
        (when should-skip-entry
          (or (outline-next-heading)
              (goto-char (point-max)))))))

  ;; org-clock-in を拡張
  ;; 発動条件1）タスクが DONE になっていないこと（変更可）
  ;; 発動条件2）アウトラインレベルが4まで．それ以上に深いレベルでは計測しない（変更可）
  (defun my:org-clock-in ()
    (setq vc-display-status nil) ;; モードライン節約
    (when (and (looking-at (concat "^\\*+ " org-not-done-regexp))
               (memq (org-outline-level) '(1 2 3 4)))
      (org-clock-in)))

  ;; org-clock-out を拡張
  (defun my:org-clock-out ()
    (setq vc-display-status t) ;; モードライン節約解除
    (when (org-clocking-p)
      (org-clock-out)))

  (defun my:org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (when (org-clocking-p)
      (org-clock-out)
      (save-some-buffers t)))

  ;; GTD settings are based on https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  ;; Add agenda files
  (let ((inbox (expand-file-name "inbox.org" my:gtd-base-path))
        (gtd (expand-file-name "gtd.org" my:gtd-base-path))
        (someday (expand-file-name "someday.org" my:gtd-base-path))
        (tickler (expand-file-name "tickler.org" my:gtd-base-path)))
    (setq org-agenda-files (list inbox gtd tickler))
    (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                   (file+headline ,inbox "Tasks")
                                   "* TODO %i%?")
                                  ("T" "Tickler" entry
                                   (file+headline ,tickler "Tickler")
                                   "* %i%? \n %U")))
    (setq org-refile-targets `((,gtd :maxlevel . 3)
                               (,someday :level . 1)
                               (,tickler :maxlevel . 2)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
    (setq org-agenda-custom-commands
          '(("o" "At the office" tags-todo "@office"
             ((org-agenda-overriding-header "Office")
              (org-agenda-skip-function #'my:org-agenda-skip-all-sibling-but-first)))))))

(use-package org-tree-slide
  :defer t
  :hook ((org-tree-slide-mode-after-narrow . my:org-clock-in)
         (org-tree-slide-before-move-next . my:org-clock-out)
         (org-tree-slide-before-move-previous . my:org-clock-out)
         (org-tree-slide-mode-stop . my:org-clock-out))
  :bind (:map org-tree-slide-mode-map
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree))
  :config
  ;; ナローイング用基本設定の適用
  (org-tree-slide-narrowing-control-profile)
  ;; 高速動作用（推奨）
  (setq org-tree-slide-modeline-display 'outside)
  ;; DONEなタスクも表示する
  (setq org-tree-slide-skip-done nil))

(use-package log4e)
(use-package org-pomodoro
  :defer t
  :bind (:map org-mode-map
              ("C-c m" . org-pomodoro)))
