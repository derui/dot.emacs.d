;;; -*- lexical-binding: t -*-
;; configurations for packages based on use-package
(eval-when-compile
  (require 'use-package))

(use-package eldoc
  :ensure nil
  :commands (eldoc-mode)
  :custom
  ;; idle時にdelayをかけない
  (eldoc-idle-delay 0)
  ;; echo areaに複数行表示を有効にする
  (eldoc-echo-area-use-multiline-p t)
  :hook ((lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

;; Enable overlay symbol on each programming mode
(use-package symbol-overlay
  :hook ((prog-mode . symbol-overlay-mode)))

(use-package s)
(use-package exec-path-from-shell
  :after (s)
  :commands (exec-path-from-shell-copy-envs)
  :config
  (mapc #'(lambda (f)
            (add-to-list 'exec-path (expand-file-name f)))
        (s-split ":" (exec-path-from-shell-getenv "PATH")))
  (let ((envs '("GOROOT" "GOPATH")))
    (exec-path-from-shell-copy-envs envs)))

(use-package beacon
  :commands (beacon-mode)
  :custom
  (beacon-color "yellow")
  :hook
  ((after-init . beacon-mode)))

(use-package imenu-list
  :custom
  (imenu-list-size 0.25)
  (imenu-list-focus-after-activation t))

(use-package which-key
  :custom
  (which-key-use-C-h-commands t)
  :hook
  ((after-init . which-key-mode)))

(use-package hydra)

(use-package smartparens
  :commands (sp-local-pair smartparens-global-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "`" nil :actions nil)
  (smartparens-global-mode 1))

;;; company
(use-package company-quickhelp
  :ensure t
  :custom
  (company-quickhelp-color-foreground "black")
  :hook ((company-mode . company-quickhelp-mode)))

(use-package company-box
  :after (all-the-icons)
  :ensure t
  :hook ((company-mode . company-box-mode))
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-doc-enable nil))

(use-package company
  :ensure t
  :commands (global-company-mode)
  :diminish (company-mode . "")
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1) ;; 1文字入力で補完されるように
  ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  :config
  (global-set-key (kbd "C-M-i") 'company-complete-common-or-cycle)
  ;; C-n, C-pで補完候補を選べるように
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; C-hがデフォルトでドキュメント表示にmapされているので、文字を消せるようにmapを外す
  (define-key company-active-map (kbd "C-h") nil)
  ;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
  (define-key company-active-map (kbd "<Tab>") 'company-complete-common-or-cycle)
  ;; ドキュメント表示
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

  ;; 色の設定。出来るだけ奇抜にならないように
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "lightgray")
  (set-face-attribute 'company-preview-common nil
                      :foreground "dark gray"
                      :background "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-selection nil
                      :background "steelblue"
                      :foreground "white")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue"
                      :underline t)
  (set-face-attribute 'company-tooltip-annotation nil
                      :foreground "red")

  (global-company-mode 1))

;; treemacs
(use-package treemacs)
(use-package treemacs-evil
  :after (treemacs))

(use-package projectile
  :commands (projectile-register-project-type)
  :hook
  ((after-init . projectile-mode))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-register-project-type
   'yarn
   '("package.json")
   :compile "yarn build"
   :test "yarn test"
   :run "yarn start"
   :test-suffix ".test"))

(use-package nyan-mode
  :custom
  (nyan-animate-nyancat t)
  :hook
  (after-init . nyan-mode))

(use-package doom-modeline
  :commands (doom-modeline-def-modeline)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline
    'main
    '(bar workspace-number window-number evil-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name debug minor-modes input-method major-mode process vcs checker)))

(use-package hide-mode-line
  :hook
  ((treemacs-mode . hide-mode-line-mode)
   (imenu-list-major-mode . hide-mode-line-mode)))

(use-package ag)
(use-package wgrep-ag
  :after (ag)
  :bind (:map ag-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :hook ((ag-mode . wgrep-ag-setup)))

(use-package fish-mode
  :mode (("\\.fish$" . fish-mode)))

;; shackleを利用する設定
(use-package shackle
  :custom
  (shackle-rules '((compilation-mode :align t :size 0.4)))
  (shackle-default-rule '(:select t))
  :hook
  ((after-init . shackle-mode)))

(use-package git-gutter
  :custom
  (git-gutter:update-hooks '(after-save-hook after-revert-hook))

  ;; 全体でgit-gutterを有効にする
  :hook ((after-init . global-git-gutter-mode)))

(use-package magit
  :hook ((git-commit-mode . my:flyspell-enable)))

;; ivy
(use-package ivy
  :diminish (ivy-mode . "")
  :custom
  (ivy-format-function 'ivy-format-function-arrow)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 30)
  (ivy-extra-directories nil)
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))

(use-package amx)
(use-package counsel
  :after (amx)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-yank-pop-separator "\n-------\n")
  (counsel-yank-pop-height 30)
  :config
  (defun my:counsel-search-dwim ()
    "Merge version to search document via grep/ag/rg.
Use fast alternative if it exists, fallback grep if no alternatives in system.
"
    (interactive)
    (cond
     ((executable-find "rg") (counsel-rg))
     ((executable-find "ag") (counsel-ag))
     (t (counsel-grep)))))

(use-package swiper
  :bind (("C-s" . swiper))
  :custom
  (swiper-include-line-number-in-search t))

(use-package migemo
  :commands (migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  ;; 遅いのを防ぐためにキャッシュする。
  (migemo-use-pattern-alist t)
  (migemo-use-frequent-pattern-alist t)
  (migemo-pattern-alist-length 1024))

(use-package avy-migemo
  :after (migemo)
  :commands (avy-migemo-mode)
  :config
  ;; 初期化する。
  (use-package avy-migemo-e.g.swiper)
  (migemo-init)
  (avy-migemo-mode 1))

(use-package ivy-rich
  :after (ivy)
  :hook ((after-init . ivy-rich-mode))
  :config
  (use-package all-the-icons :commands (all-the-icons-icon-for-mode))

  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon :width 2)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand)))))

;; mozc
(when (boundp 'my:mozc-helper-locate)

  (use-package mozc
    :custom
    (mozc-candidate-style 'echo-area)
    (mozc-helper-program-name my:mozc-helper-locate))

  (use-package mozc-popup
    :after (mozc)
    :custom
    ;; popup スタイルを使用
    (mozc-candidate-style 'popup)))

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package popup :commands (popup-tip))
(use-package langtool
  :after (popup)
  :commands (langtool-details-error-message)
  :custom
  (langtool-language-tool-jar my:langtool-cli-path)
  (langtool-default-language "en-US")
  (langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
  :preface
  (defun my:langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g' .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  :config
  (setq langtool-autoshow-message-function #'my:langtool-autoshow-detail-popup))

(use-package auto-save-buffers-enhanced
  :custom
  (auto-save-buffers-enhanced-interval 3.0)
  :config
  (auto-save-buffers-enhanced t))

(use-package flycheck)
(use-package flycheck-posframe
  :after (flycheck)
  :hook ((flycheck-mode . flycheck-posframe-mode)))

;;; org-mode
(use-package org
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
    (setq org-capture-templates
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

  (defun my:org-remove-task-time-from-mode-line ()
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


;;; sdic-modeで利用する設定を記述する。
(use-package sdic
  :defer t
  :config
  (setq default-fill-column 80)
  (setq sdicf-array-command "/usr/bin/sary") ; コマンドパス
  (setq sdic-eiwa-dictionary-list
        `((sdicf-client ,(locate-user-emacs-file "etc/dict/eijiro.sdic") (strategy array)))
        sdic-waei-dictionary-list
        `((sdicf-client ,(locate-user-emacs-file "etc/dict/waeijiro.sdic") (strategy array))))

  ;; saryを直接使用できるように sdicf.el 内に定義されているarrayコマンド用関数を強制的に置換
  (fset 'sdicf-array-init 'sdicf-common-init)
  (fset 'sdicf-array-quit 'sdicf-common-quit)
  (fset 'sdicf-array-search
        (lambda (sdic pattern &optional case regexp)
          (sdicf-array-init sdic)
          (if regexp
              (signal 'sdicf-invalid-method '(regexp))
            (save-excursion
              (set-buffer (sdicf-get-buffer sdic))
              (delete-region (point-min) (point-max))
              (apply 'sdicf-call-process
                     sdicf-array-command
                     (sdicf-get-coding-system sdic)
                     nil t nil
                     (if case
                         (list "-i" pattern (sdicf-get-filename sdic))
                       (list pattern (sdicf-get-filename sdic))))
              (goto-char (point-min))
              (let (entries)
                (while (not (eobp)) (sdicf-search-internal))
                (nreverse entries))))))

  (defadvice sdic-forward-item (after sdic-forward-item-always-top activate)
    (recenter 0))
  (defadvice sdic-backward-item (after sdic-backward-item-always-top activate)
    (recenter 0))

  (setq sdic-default-coding-system 'utf-8-unix)

  ;; 文字色
  (setq sdic-face-color "pink")

  ;; 辞書ファイルの設定
  (setq sdic-inline-eiwa-dictionary
        (concat user-emacs-directory "etc/dict/gene.sdic"))
  (setq sdic-inline-waei-dictionary
        (concat user-emacs-directory "etc/dict/jedict.sdic"))
  ;; デフォルト値。ポイント位置が前回と同じである限り、再度辞書ではひかない。
  (setq sdic-inline-not-search-style 'point))

;;; evil
(use-package evil-cleverparens
  :after (evil)
  :commands (evil-cleverparens-mode))

(use-package evil-mc
  :after (evil hydra)
  :hook ((after-init . global-evil-mc-mode))
  :config
  (defhydra hydra-evil-mc (:hint nil)
    "
 Up^^             Down^^           Miscellaneous
---------------------------------------------------
 [_k_]   Next     [_j_]   Next     [_a_] Mark all
 [_K_]   Skip     [_J_]   Skip     [_c_] Clear all
 [_g_]  First     [_G_]   Last     [_q_] Quit
 "
    ("a" evil-mc-make-all-cursors :exit t)
    ("j" evil-mc-make-and-goto-next-match)
    ("J" evil-mc-skip-and-goto-next-match)
    ("k" evil-mc-make-and-goto-prev-match)
    ("K" evil-mc-skip-and-goto-prev-match)
    ("g" evil-mc-make-and-goto-first-cursor)
    ("G" evil-mc-make-and-goto-last-cursor)
    ("c" evil-mc-undo-all-cursors :exit t)
    ("q" nil)))

(use-package evil-leader
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "p" 'projectile-command-map
    "i" 'hydra-evil-mc/body
    "q" 'evil-delete-buffer
    "w" 'save-buffer
    "oc" 'org-capture
    "os" 'org-tree-slide-mode
    "d" 'dired-jump
    "e" 'find-file
    "b" 'ibuffer
    "#" 'server-edit
    "s" 'my:counsel-search-dwim
    "m" 'magit-status
    "f" 'counsel-git
    "tt" 'treemacs-select-window
    "tq" 'treemacs-quit
    ;; 'l' is head character of operations for 'lint'
    ;; Recommend to use evil's default keybinding (z =, s ] or s [) when correct warning issued from flyspell.
    "ll" 'langtool-check
    "lL" 'langtool-check-done
    ;; 'c' is head character of 'counsel'
    "ci" 'counsel-imenu
    "cg" 'counsel-git-grep
    "cs" 'counsel-ag
    "cf" 'counsel-git
    "x" 'counsel-M-x
    "z" 'winner-undo)

  ;; set up key binding for org-mode local with evil-leader
  (evil-leader/set-key-for-mode 'org-mode
    ",a" 'org-agenda
    ",n" 'org-narrow-to-subtree
    ",w" 'widen
    ",p" 'org-pomodoro)
  )

(use-package ace-window :defer t)
(use-package avy :defer t)
(use-package evil
  :commands (evil-ex-define-cmd evil-set-initial-state evil-normal-state-p evil-insert-state)
  :hook (;; Disable ime returned to normal state
         (evil-normal-state-entry . my:evil-disable-ime))

  :bind (:map
         evil-normal-state-map
         ("M-y" . counsel-yank-pop)
         ("s" . nil)
         (";" . ivy-switch-buffer)
         ("C-a" . evil-numbers/inc-at-pt)
         ("C-x" . evil-numbers/dec-at-pt)
         ("TAB" . nil)
         ;; evil-jump-forwardを潰す。
         :map evil-motion-state-map
         ("TAB" . nil)
         ("s f" . avy-goto-char)
         ("s j" . avy-goto-line-below)
         ("s k" . avy-goto-line-above)

         :map evil-insert-state-map
         ("M-y" . counsel-yank-pop)
         ("C-q" . evil-normal-state)
         ("<Hangul>" . my:evil-enable-ime)
         ("<henkan>" . my:evil-enable-ime)
         ("<Hangul_Hanja>" . my:evil-disable-ime)
         ("<muhenkan>" . my:evil-disable-ime)

         :map evil-window-map
         ("C-w" . ace-select-window))

  :preface
  (defun my:evil-change-input-method (ime-state)
    (let ((when-emacs-state (string= evil-state "emacs")))
      (cond
       ((and ime-state (or (not current-input-method) (string-equal current-input-method my:input-method)))
        (set-input-method my:input-method)
        (when (evil-normal-state-p)
          (evil-insert-state)))
       (t
        (set-input-method nil)))))

  (defun my:evil-enable-ime ()
    (interactive)
    (my:evil-change-input-method t))

  (defun my:evil-disable-ime ()
    (interactive)
    (my:evil-change-input-method nil))

  (defun my:evil-swap-key (map key1 key2)
    ;; MAP中のKEY1とKEY2を入れ替え
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))

  :config
  (use-package evil-numbers :commands (evil-numbers/dec-at-pt evil-numbers/inc-at-pt))

  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

  (setcdr evil-insert-state-map nil)
  ;;M-:
  (evil-ex-define-cmd "eval" 'eval-expression)
  (evil-ex-define-cmd "ev" "eval")

  ;;C-h k
  (evil-ex-define-cmd "describe-key" 'describe-key)
  (evil-ex-define-cmd "key" "describe-key")

  ;; 論理行と物理行の移動を入れ替え
  (my:evil-swap-key evil-motion-state-map "j" "gj")
  (my:evil-swap-key evil-motion-state-map "k" "gk")

  (setq evil-normal-state-tag   (propertize "N" 'face '((:foreground "black")))
        evil-emacs-state-tag    (propertize "E" 'face '((:foreground "black")))
        evil-insert-state-tag   (propertize "I" 'face '((:foreground "red")))
        evil-motion-state-tag   (propertize "M" 'face '((:foreground "blue")))
        evil-visual-state-tag   (propertize "V" 'face '((:foreground "black")))
        evil-operator-state-tag (propertize "O" 'face '((:foreground "purple"))))

  ;; To suppress error when exit from insert-state
  (setq abbrev-expand-function #'ignore)
  (evil-mode 1))

;;; yasnippet

;; (@* "yasnippetを利用する設定")
(use-package s :commands s-join)
(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("<C-tab>" . yas-expand))
  :commands (yas-expand yas-global-mode)
  :hook ((after-init . yas-global-mode))
  :config
  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (let ((group-max-len 0)
          (key-max-len 0)
          (fmt "")
          (popup-items))

      (mapc #'(lambda (choice)
                (when (yas--template-p choice)
                  (setq group-max-len (max group-max-len
                                           (+ (length (yas--template-group choice) )
                                              (apply '+ (mapcar 'length (yas--template-group choice))))))
                  (setq key-max-len (max key-max-len (length (yas--template-key choice))))))
            choices)

      (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds  %%s"
                        (if (> group-max-len 0 ) "" " ")
                        group-max-len group-max-len
                        (if (> group-max-len 0 ) " > " "")
                        key-max-len key-max-len))

      (setq popup-items
            (mapcar
             #'(lambda (choice)
                 (popup-make-item
                  (if (yas--template-p choice)
                      (format fmt
                              (if (yas--template-group choice)
                                  (s-join "/" (yas--template-group choice))
                                "")
                              (if (yas--template-key choice)
                                  (yas--template-key choice)
                                "")
                              (if (yas--template-name choice)
                                  (yas--template-name choice)
                                ""))
                    (format " %s" choice))
                  :value choice))
             choices))

      (popup-menu*
       popup-items
       :prompt prompt
       :max-width 80
       :isearch t)))

  (setq yas-prompt-functions '(yas-popup-isearch-prompt)))

;; googleのコーティング規約に依存するための設定
(use-package google-c-style
  ;; .hはc++-modeで開く
  :mode (("\\.h$" . c++-mode))
  :preface
  (defun my:c-mode-hook ()
    (setq completion-mode t)
    ;; compile-windowの設定
    (setq compilation-buffer-name "*compilation*")
    (setq compilation-scroll-output t)
    (setq compilation-read-command t)
    (setq compilation-ask-about-save nil)
    (setq compilation-window-height 10)
    (setq compile-command "make")
    ;; cc-mode内で定義されるキーバインド
    (define-key c-mode-base-map (kbd "C-c C-c")   'comment-region)
    (define-key c-mode-base-map (kbd "C-c C") 'my-c++-cast)
    (define-key c-mode-base-map (kbd "C-c C-M-c") 'uncomment-region)
    (define-key c-mode-base-map (kbd "C-c e")      'c-macro-expand)
    (define-key c-mode-base-map (kbd "C-c c")      'my-compile)
    (define-key c-mode-base-map (kbd "C-c M-c")   'compilation-close)
    (define-key c-mode-base-map (kbd "C-c g")      'gdb)
    (define-key c-mode-base-map (kbd "C-c t")      'toggle-source)
    (define-key c-mode-base-map (kbd "C-c C-d") 'c-down-conditional)
    ;; cc-modeに入る時に自動的にgtags-modeにする
    (gtags-mode t))

  :hook ((c-mode-common . my:c-mode-hook)
         (c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

;;; golang
(use-package company-go
  :after (company)
  :commands (company-go))

(use-package go-eldoc
  :commands (go-eldoc-setup))

(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump))
  :hook ((go-mode . my:go-mode-hook-0))
  :preface
  (defun my:go-mode-hook-0 ()
    (setq-local tab-width 2)
    (setq-local company-backends '(company-go))

    (company-mode 1)
    (go-eldoc-setup)))

;;; lsp
(use-package lsp-mode
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  ;; do not use flymake
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 'incremental) ;; always send incremental document
  (lsp-response-timeout 5)
  (lsp-enable-completion-at-point nil)
  :bind
  (:map lsp-mode-map
        ("C-c r"   . lsp-rename)
        ("<Tab>" . company-indent-or-complete-common))
  :hook
  (lsp-mode . my:lsp-disable-eldoc-when-hover)
  :config
  (require 'lsp-clients)

  (defun my:lsp-disable-eldoc-when-hover ()
    (when (my:minor-mode-active-p 'lsp-mode)
      (setq-local eldoc-message-function (lambda (&rest _) (progn)))))

  ;; LSP UI tools
  (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    ;; lsp-ui-flycheck
    ;; use flycheck on the fly
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions nil)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable nil)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'always) ;; never, on-demand, or always
    :preface
    (defun my:toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-j" . lsp-ui-peek-find-definitions)
          ("C-c i"   . lsp-ui-peek-find-implementation)
          ("C-c m"   . imenu-list-smart-toggle)
          ("C-c s"   . lsp-ui-sideline-mode)
          ("C-c d"   . my:toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode)
    :config
    (ad-disable-regexp "lsp-ui-doc-.+")
    (ad-activate 'select-window))

  ;; Lsp completion
  (use-package company-lsp
    :after (company)
    :custom
    (company-lsp-cache-candidates 'auto) ;; always using cache
    (company-lsp-async t)
    (company-lsp-enable-recompletion nil)))

;;; rust
(use-package rust-mode
  :custom
  (rust-indent-offset 4)
  (racer-rust-src-path my:rust-src-location)
  (racer-cmd my:rust-racer-path)
  :hook ((rust-mode . racer-mode)
         (rust-mode . eldoc-mode)))

;;; python

(use-package python
  :mode ("\\.py$" . python-mode)
  :hook ((python-mode . elpy-mode))
  :config

  (use-package elpy
    :after (flycheck)
    :preface
    (defun my:elpy-mode-hook-0 ()
      (setq-local indent-tabs-mode nil)
      (flycheck-mode))
    :custom
    (elpy-rpc-backend "jedi")
    ;; use jedi via company-mode
    (jedi:complete-on-dot t)
    :hook ((elpy-mode . my:elpy-mode-hook-0))
    :config
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (elpy-enable))

  (pyvenv-activate my:virtualenv-path))

;;; ruby
;; (@* "ruby関連の設定")
(use-package ruby-mode
  :mode ("\\.rb$" . ruby-mode)
  :bind (:map ruby-mode-map
              ("C-c x" . xmp)
              ("C-M-i" . rct-complete-symbol--anything)))

(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode))

;;; Common Lisp

;;; (@* "common lisp関係の設定")
(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(use-package lisp-mode
  :preface
  (defun my:lisp-hooks ()
    (setq-local company-idle-delay 0.2)
    (setq-local show-paren-style 'expression))

  :hook ((lisp-mode . my:lisp-hooks)
         (lisp-mode . aggressive-indent-mode)
         (lisp-mode . evil-cleverparens-mode)
         (lisp-mode . set-newline-and-indent)))

(let ((helper (expand-file-name "helper.el" my:roswell-path)))
  (when (and (file-exists-p helper)
             my:roswell-path)
    (defvar roswell-slime-contribs '(slime slime-fancy slime-company))
    (load helper)

    (defun slime-qlot-exec (directory)
      "start slime with qlot"
      (slime-start :program "qlot"
                   :program-args '("exec" "ros" "-S" "." "run")
                   :directory directory
                   :name 'qlot
                   :env (list (concat "PATH="
                                      (mapconcat 'identity exec-path ":"))
                              (concat "QUICKLISP_HOME="
                                      (file-name-as-directory directory) "quicklisp/"))))

    (defun slime-qlot (directory)
      "start slime with qlot"
      (interactive (list (read-directory-name "Project directory: ")))
      (slime-qlot-exec directory))

    (defun slime-qlot-restart (directory)
      (interactive (list (read-directory-name "Project directory: ")))
      (ignore-errors
        (let* ((buffer (get-buffer "*inferior-lisp*"))
               (process (get-buffer-process buffer)))
          (when (and buffer process)
            (set-process-query-on-exit-flag process nil)
            (kill-buffer buffer))))
      (slime-qlot-exec directory))))

(use-package hyperspec
  :when (featurep 'slime)
  :ensure nil
  :custom
  ;; HyperSpecをewwで見る設定
  (common-lisp-hyperspec-root "~/.emacs.d/share/HyperSpec/")

  :config
  (unless (file-exists-p (expand-file-name "~/.emacs.d/share/HyperSpec"))
    (when (eq window-system 'x)

      (make-directory (expand-file-name "~/.emacs.d/share") t)
      (let ((hyperspec-url "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
            (output "/tmp/HyperSpec.tar.gz"))

        (call-process "curl" nil nil t "-L" "-o" output hyperspec-url)
        (shell-command (format "tar zxvf %s -C %s" output "~/.emacs.d/share")))))

  ;; redefine function
  (defun common-lisp-hyperspec (symbol-name)
    (interactive (list (common-lisp-hyperspec-read-symbol-name)))
    (let ((buf (current-buffer)))
      (let ((name (common-lisp-hyperspec--strip-cl-package
                   (downcase symbol-name))))
        (cl-maplist (lambda (entry)
                      (eww-open-file (concat common-lisp-hyperspec-root "Body/"
                                             (car entry)))
                      (when (cdr entry)
                        (sleep-for 1.5)))
                    (or (common-lisp-hyperspec--find name)
                        (error "The symbol `%s' is not defined in Common Lisp"
                               symbol-name))))
      (switch-to-buffer buf)
      (display-buffer "*eww*"))))

;;; emacs lisp

;;; (@* "elisp関係の設定")
;; 保存された場合に、自動的にバイトコンパイルを行うための設定
;; from rubikitch
(use-package elisp-mode
  :after (auto-async-byte-compile)
  :ensure nil
  :preface
  (defun my:emacs-lisp-hooks ()
    (setq-local company-idle-delay 0.2)
    (setq-local company-backends '(company-semantic company-files company-elisp))
    (setq-local show-paren-style 'expression)

    (company-mode 1))

  :hook ((emacs-lisp-mode . my:emacs-lisp-hooks)
         (emacs-lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . evil-cleverparens-mode)
         (emacs-lisp-mode . enable-auto-async-byte-compile-mode)
         (emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . set-newline-and-indent)))

(use-package auto-async-byte-compile)

;;; OCaml
(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)
         ("dune" . tuareg-dune-mode))
  :custom
  ;; Global tuareg setting
  (tuareg-let-always-indent t)
  (tuareg-function-indent 0)
  (tuareg-match-indent 0)
  (tuareg-sig-struct-indent 0)
  (tuareg-match-patterns-aligned t)
  :hook ((tuareg-mode . tuareg-mode-hook-1))
  :bind (:map tuareg-mode-map
              ("C-c f" . ocp-indent-buffer))
  :config
  (setq tuareg-begin-indent 'tuareg-default-indent)

  (eval-and-compile
    (defun my:opam-share-directory-p ()
      (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
        (and opam-share (file-directory-p opam-share))))

    (defun my:opam-load-path ()
      (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
        (when (and opam-share (file-directory-p opam-share))
          (expand-file-name "emacs/site-lisp" opam-share)))))

  ;; Load merlin-mode
  (when (my:opam-share-directory-p)
    (add-to-list 'load-path (my:opam-load-path))
    (require 'merlin)
    (require 'merlin-company)
    (require 'merlin-imenu)

    (setq merlin-command 'opam)

    (require 'ocp-indent)
    (autoload 'ocp-indent-buffer "ocp-indent" nil t)

    (require 'ocamlformat nil t))

  (defun tuareg-mode-hook-1 ()
    ;; indentation rules

    (setq-local company-backends '(company-semantic company-files merlin-company-backend))
    (setq-local company-idle-delay 0.2)

    ;; ocamlspot and other keys
    (electric-indent-mode 1)
    (merlin-mode 1)
    (merlin-use-merlin-imenu)

    (when (featurep 'flyspell)
      (flyspell-prog-mode))

    (when (featurep 'ocamlformat)
      (add-hook 'before-save-hook #'ocamlformat-before-save nil t))
    (when (featurep 'ocp-indent)
      (add-hook 'before-save-hook #'ocp-indent-buffer t t))))

;; Asciidoc
(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

;;; Lua
(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

;;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))


;;; Restructured Text
(use-package rst
  :mode ("\\.rst\\'" . rst-mode))

;;; web
(use-package company-css
  :commands (company-css))

(use-package rainbow-mode
  :commands (rainbow-mode))

(use-package css-mode
  :mode ("\\.scss" . scss-mode)
  :custom
  (scss-compile-at-save nil)
  :config
  (defun my:scss-mode-hook-0 ()
    (setq-local css-indent-offset 2)
    (setq-local company-backends '(company-semantic
                                   company-files
                                   company-css))
    (rainbow-mode))
  (add-hook 'scss-mode-hook 'my:scss-mode-hook-0))

;; yaml
(use-package yaml-mode
  :mode ("\\.yml" . yaml-mode))

;; web-mode
(use-package web-mode
  :mode
  ("\\.html" . web-mode)
  ("\\.rt" . web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :preface
  (defun my:web-mode-hook-0 ())
  :config
  (add-hook 'web-mode-hook #'my:web-mode-hook-0))

;; stylus-mode
(use-package stylus-mode
  :mode ("\\.styl$" . stylus-mode)
  :preface
  (defun my:stylus-mode-hook-0 ())
  :config
  (add-hook 'stylus-mode-hook #'my:stylus-mode-hook-0))

;;; closure
(use-package clojure-mode :defer t
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojure-mode . clj-refactor-mode)
         (clojure-mode . my:clojure-mode-hook-0))
  :config
  (use-package clj-refactor)
  (use-package cider
    :hook ((cider-mode . eldoc-mode)
           (cider-mode . my:cider-mode-hook-0))
    :custom
    (cider-repl-display-in-current-window t)
    (cider-repl-use-clojure-font-lock t)
    (cider-save-file-on-load 'always-save)
    (cider-font-lock-dynamically '(macro core function var))
    (cider-overlays-use-font-lock t)
    :config
    (cider-repl-toggle-pretty-printing)

    )
  (defun my:clojure-mode-hook-0 ()
    (cljr-add-keybindings-with-prefix "C-c j"))

  ;; cider
  (defun my:cider-mode-hook-0 ()))

;;; JavaScript
(use-package prettier-js
  :custom
  ;; do not show error
  (prettier-js-show-errors nil))

(use-package add-node-modules-path)

(use-package js2-mode
  :hook ((js-mode . js2-minor-mode)
         (js-mode . flycheck-mode)
         (js-mode . my:js2-mode-hook))
  :mode
  ("\\.js" . js2-mode)
  ("\\.es6" . js2-mode)
  :config
  (defun my:js2-mode-hook ()
    (setq-local js2-bounce-indent-p nil)
    (setq-local js2-basic-offset 2)
    (setq-local js2-include-browser-externs nil)
    (setq-local js2-mode-show-parse-errors nil)
    (setq-local js2-mode-show-strict-warnings nil)
    (setq-local js2-highlight-external-variables nil)
    (setq-local js2-include-jslint-globals nil)))

(use-package rjsx-mode
  :commands (rjsx-mode)
  :hook ((rjsx-mode . my:rjsx-mode-hook))
  :mode
  ("components\\/.*\\.js\\'" . rjsx-mode)
  ("containers\\/.*\\.js\\'" . rjsx-mode)
  :config
  (defun my:rjsx-mode-hook ()
    (flycheck-mode)))

;;; Terraform
(use-package terraform-mode
  :ensure t
  :mode (("\\.tf$" . terraform-mode)))

;;; Typescript

(use-package typescript-mode
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . web-mode))
  :hook ((web-mode . my:web-mode-hook-enable-jsx)
         (typescript-mode . my:typescript-mode-hook))
  :custom
  (typescript-indent-level 2)
  :config
  (defun my:web-mode-hook-enable-jsx ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setq-local web-mode-enable-auto-quoting nil)
      (my:typescript-mode-hook)))

  (defun my:typescript-mode-hook ()
    (add-node-modules-path)

    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (setq-local flycheck-javascript-eslint-executable "eslint_d")
    (setq-local prettier-js-args '("--parser" "typescript" "--pkg-conf"))
    (setq-local prettier-js-command (cond
                                     ((executable-find "prettier_d") "prettier_d")
                                     (t "prettier")))
    (setq-local company-backends '(company-semantic company-files company-lsp))

    (prettier-js-mode +1)
    (company-mode +1)
    (flycheck-mode +1)
    (lsp))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

;;; plantuml

(use-package plantuml-mode
  :custom
  (plantuml-output-type "png")
  (plantuml-options "-charset UTF-8")
  :config
  (let ((plantuml-jar-file (expand-file-name (locate-user-emacs-file "plantuml.jar"))))
    (setq plantuml-jar-path plantuml-jar-file)
    (unless (file-exists-p plantuml-jar-file)
      (call-process "curl" nil nil t "-L" "-o" plantuml-jar-file
                    "https://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))))
