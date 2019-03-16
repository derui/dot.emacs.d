;;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'use-package))

(use-package org
  :after (evil-leader)
  :mode ("\\.org$" . org-mode)
  :hook ((org-mode . turn-on-font-lock))
  :custom
  ;; org-mode内部のソースを色付けする
  (org-src-fontify-natively t)
  ;; org-modeの開始時に、行の折り返しを無効にする。
  (org-startup-truncated t)
  ;; follow-linkから戻ることを可能とする。
  (org-return-follows-link t)

  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-agenda-custom-commands
   '(("o" "At the office" tags-todo "@office"
      ((org-agenda-overriding-header "Office")
       (org-agenda-skip-function #'my:org-agenda-skip-all-sibling-but-first)))))

  (org-indent-indentation-per-level 0)
  (org-adapt-indentation nil)
  (org-agenda-current-time-string "← now")
  (org-agenda-time-grid ;; Format is changed from 9.1
   '((daily today require-timed)
     (0700 0800 0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     "-"
     "────────────────"))
  :config
  ;; 一時間に一回、org-modeの全てのバッファを保存する。
  (run-at-time "00:59" 3600 #'org-save-all-org-buffers)

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

  (defun my:org-global-props (&optional property buffer)
    "Get the plists of global org properties of current buffer."
    (unless property (setq property "PROPERTY"))
    (with-current-buffer (or buffer (current-buffer))
      (org-element-map
          (org-element-parse-buffer)
          'keyword
        (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

  (defun my:org-add-ymd-to-archive (name)
    "replace anchor to YYYY-MM string"
    (let* ((ymd (format-time-string "%Y-%m")))
      (replace-regexp-in-string "#YM" ymd name)))
  (advice-add 'org-extract-archive-file :filter-return #'my:org-add-ymd-to-archive)

  ;; GTD settings are based on https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  ;; Add agenda files
  (let ((inbox (expand-file-name "inbox.org" my:gtd-base-path))
        (gtd (expand-file-name "gtd.org" my:gtd-base-path))
        (someday (expand-file-name "someday.org" my:gtd-base-path))
        (tickler (expand-file-name "tickler.org" my:gtd-base-path)))
    (setq org-agenda-files (list inbox gtd tickler))
    (org-capture-upgrade-templates
     `(("t" "Todo [inbox]" entry
        (file+headline ,inbox "Tasks")
        "* TODO %i%?")
       ("T" "Tickler" entry
        (file+headline ,tickler "Tickler")
        "* %i%? \n %U")))

    (setq org-refile-targets `((,gtd :maxlevel . 3)
                               (,someday :level . 1)
                               (,tickler :maxlevel . 2)))
    ))

(use-package org-bullets
  :after (org)
  :custom (org-bullets-bullet-list '("" "" "" "" "" "" ""))
  :hook ((org-mode . org-bullets-mode)))

(defvar my:org-clocked-time-mode-line "")
(put 'my:org-clocked-time-mode-line 'risky-local-variable t)

(use-package org-clock
  :after (org)
  :hook ((kill-emacs . my:org-clock-out-and-save-when-exit))
  :custom
  (org-clock-clocked-in-display 'none)
  (org-clock-out-remove-zero-time-clocks t)
  :config
  (defun my:task-clocked-time ()
    (interactive)
    (let* ((clocked-time (org-clock-get-clocked-time))
           (h (truncate clocked-time 60))
           (m (mod clocked-time 60))
           (work-done-str (format "%d:%02d" h m)))
      (if org-clock-effort
          (let* ((effort-in-minutes
                  (org-duration-to-minutes org-clock-effort))
                 (effort-h (truncate effort-in-minutes 60))
                 (effort-m (truncate (mod effort-in-minutes 60)))
                 (effort-str (format "%d:%02d" effort-h effort-m)))
            (format "%s/%s" work-done-str effort-str))
        (format "%s" work-done-str))))

  (defun my:update-task-clocked-time ()
    (setq my:org-clocked-time-mode-line (my:task-clocked-time)))

  ;; org-clock-in を拡張
  ;; 発動条件1）タスクが DONE になっていないこと（変更可）
  ;; 発動条件2）アウトラインレベルが4まで．それ以上に深いレベルでは計測しない（変更可）
  (defun my:org-clock-in ()
    (when (and (looking-at (concat "^\\*+ " org-not-done-regexp))
               (memq (org-outline-level) '(1 2 3 4)))
      (org-clock-in)))

  ;; org-clock-out を拡張
  (defun my:org-clock-out ()
    (when (org-clocking-p)
      (org-clock-out)))

  (defun my:org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (when (org-clocking-p)
      (org-clock-out)
      (save-some-buffers t))))

(use-package org-tree-slide
  :after (org org-clock)
  :hook ((org-tree-slide-before-narrow. my:org-clock-in)
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

(use-package org-pomodoro
  :after (org-agenda notifications)
  :custom
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-format "%s")
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format  "%s")
  :hook
  (org-pomodoro-started . my:org-add-task-time-to-mode-line)
  (org-pomodoro-finished . my:org-remove-task-time-from-mode-line)
  (org-pomodoro-tick . my:update-task-clocked-time)

  (org-pomodoro-started . (lambda () (notifications-notify
                                      :title "org-pomodoro"
                                      :body "Let's focus for 25 minutes!")))
  (org-pomodoro-finished . (lambda () (notifications-notify
                                       :title "org-pomodoro"
                                       :body "Well done! Take a break."
                                       )))
  (org-pomodoro-short-break-finished . (lambda () (notifications-notify
                                                   :title "org-pomodoro"
                                                   :body "Finish short break. Will do next round!")))
  (org-pomodoro-long-break-finished . (lambda () (notifications-notify
                                                  :title "org-pomodoro"
                                                  :body "Finish long break.")))
  :config
  (defun my:org-add-task-time-to-mode-line ()
    (add-to-list 'global-mode-string 'my:org-clocked-time-mode-line t))

  (defun my:org-remove-task-time-from-mode-line-hook ()
    (when (memq 'my:org-clocked-time-mode-line global-mode-string)
      (setq global-mode-string
            (remove 'my:org-clocked-time-mode-line global-mode-string))))

  :bind (:map org-agenda-mode-map
              ("P" . org-pomodoro)))

(use-package ox-hugo
  :after (org)
  :hook ((org-mode . my:org-hugo-enable-if-hugo-buffer))
  :config
  (defun my:org-hugo-enable-if-hugo-buffer ()
    (let ((prop (my:org-global-props "HUGO_.\+" (current-buffer))))
      (when prop
        (org-hugo-auto-export-mode +1)))))
