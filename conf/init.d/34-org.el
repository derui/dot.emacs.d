(eval-when-compile
  (require 'use-package))

(use-package org
  :ensure nil
  :mode ("\\.org$" . org-mode)
  :hook ((org-mode-hook . turn-on-font-lock))
  :config
  ;; org-mode内部のソースを色付けする
  (setq org-src-fontify-natively t)

  ;; 一時間に一回、org-modeの全てのバッファを保存する。
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)

  ;; org-modeの開始時に、行の折り返しを無効にする。
  (setq org-startup-truncated t)
  ;; follow-linkから戻ることを可能とする。
  (setq org-return-follows-link t)

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

  ;; GTD settings are based on https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  ;; Add agenda files
  (let ((inbox (expand-file-name "inbox.org" my:gtd-base-path))
        (gtd (expand-file-name "gtd.org" my:gtd-base-path))
        (someday (expand-file-name "someday.org" my:gtd-base-path))
        (tickler (expand-file-name "tickler.org" my:gtd-base-path)))
    (setq org-agenda-files (list inbox gtd tickler))
    (setq org-capture-templates `(("t" "Todo [inbox]" entry
                                   (file+headfile ,inbox)
                                   "* TODO %i%?")
                                  ("T" "Tickler" entry
                                   (file+headline ,tickler "Tickler")
                                   "* %i%? \n %U")))
    (setq org-refile-targets `((,gtd :maxlevel . 3)
                               (,someday :level . 1)
                               (,tickler :maxlevel . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
    (setq org-agenda-custom-commands
          '(("o" "At the office" tags-todo "@office"
             ((org-agenda-overriding-header "Office")
              (org-agenda-skip-function #'my:org-agenda-skip-all-sibling-but-first))))))
  )

(use-package log4e)
(use-package org-pomodoro
  :defer t
  :bind (:map org-mode-map
              ("C-c m" . org-pomodoro)))
