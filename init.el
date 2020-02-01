;; -*- coding: utf-8 -*-

;; 各elispの構成は以下のとおりとする。
;; init.el el-getの初期設定と、基底となるディレクトリを変更する。
;;     conf/      -- 各種設定ファイルを格納
;;       site-lisp -- el-getで管理しない(できない)ようなelispを格納する
;;       init.d/   -- 各種初期化ファイルを格納する。
;;     etc/ -- 各種elispで利用するデータなどを格納する
;;     el-get/ -- el-getでインストールしたelispを格納する
;; init.elについては、各elispで利用する基本的な変数群と、loadpathなどの環境変数の設定を行う。

;; emacs -l init.elのように起動された場合の、user-emacs-directoryの設定

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (unless package-archive-contents (package-refresh-contents))

(eval-when-compile
  (require 'cl-lib))

;; 設定ファイルの基準となるディレクトリを、init.elのあるディレクトリとする
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)

(defun my:get-recuresive-directories (file-list)
  "Get file path list recuresively."
  (let ((path-list nil))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (cl-loop for x
             in file-list
             do (when (file-directory-p x)
                  (message x)
                  (progn
                    (setq path-list (push x path-list))

                    (setq path-list
                          (append
                           (my:get-recuresive-directories
                            (cl-remove-if
                             (lambda (x) (not (file-directory-p x)))
                             (cl-remove-if
                              (lambda(y) (string-match "\\.$\\|\\.svn\\|~$\\|\\.git\\$" y))
                              (directory-files x t))))
                           path-list)))))
    path-list))


;; 起動時間の計測関係
(defconst my:time-zero (current-time))
(defvar my:time-list nil)

(defun my:time-lag-calc (lag label)
  (if (assoc label my:time-list)
      (setcdr (assoc label my:time-list)
              (- lag (cdr (assoc label my:time-list))))
    (setq my:time-list (cons (cons label lag) my:time-list))))

(defun my:time-lag (label)
  (let* ((now (current-time))
         (min (- (car now) (car my:time-zero)))
         (sec (- (car (cdr now)) (car (cdr my:time-zero))))
         (msec (/ (- (car (cdr (cdr now)))
                     (car (cdr (cdr my:time-zero))))
                  1000))
         (lag (+ (* 60000 min) (* 1000 sec) msec)))
    (my:time-lag-calc lag label)))

(defun my:time-lag-print ()
  (message (prin1-to-string
            (sort my:time-list
                  (lambda  (x y)  (> (cdr x) (cdr y)))))))

(my:time-lag "total")

(add-hook 'after-init-hook #'(lambda () (my:time-lag "total") (my:time-lag-print)) t)

(setq load-path (append load-path
                        (my:get-recuresive-directories (locate-user-emacs-file "conf/site-lisp"))))

;; 一連の初期化処理を動かす
(let* ((conf-list '("conf/user-env"
                    "conf/emacs-base-setting"
                    "conf/user-defined"
                    "conf/os-setting"
                    "conf/package-config")))
  (dolist (conf conf-list)
    (load (expand-file-name conf user-emacs-directory))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 2)
 '(ac-ispell-requires 4)
 '(ag-highlight-search t)
 '(ag-reuse-buffers 'nil)
 '(ag-reuse-window 'nil)
 '(all-the-icons-scale-factor 1.0 t)
 '(auto-save-buffers-enhanced-interval 3.0 t)
 '(avy-migemo-function-names
   '((ivy--regex-ignore-order :around ivy--regex-ignore-order-migemo-around)
     swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full))
 '(beacon-color "yellow" t)
 '(cider-font-lock-dynamically '(macro core function var) t)
 '(cider-overlays-use-font-lock t t)
 '(cider-repl-display-in-current-window t t)
 '(cider-repl-use-clojure-font-lock t t)
 '(cider-save-file-on-load 'always-save t)
 '(common-lisp-hyperspec-root "~/.emacs.d/share/HyperSpec/" t)
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(company-idle-delay 0.5 t)
 '(company-lsp-async t t)
 '(company-lsp-cache-candidates 'auto t)
 '(company-lsp-enable-recompletion t t)
 '(company-minimum-prefix-length 1 t)
 '(company-quickhelp-color-foreground "black")
 '(company-selection-wrap-around t t)
 '(company-tooltip-align-annotations t t)
 '(counsel-yank-pop-separator "
-------
" t)
 '(custom-safe-themes
   '("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" default))
 '(dashboard-items '((recents . 15) (projects . 5) (agenda . 5)) t)
 '(dashboard-startup-banner 4 t)
 '(doom-modeline-buffer-file-name-style 'truncate-with-project t)
 '(doom-modeline-icon t t)
 '(doom-modeline-major-mode-icon nil t)
 '(doom-modeline-minor-modes nil t)
 '(eldoc-echo-area-use-multiline-p t)
 '(eldoc-idle-delay 0)
 '(enable-recursive-minibuffers t)
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
 '(git-gutter:update-hooks '(after-save-hook after-revert-hook) t)
 '(google-translate-translation-directions-alist '(("ja" . "en") ("en" . "ja")) t)
 '(helm-ls-git-default-sources '(helm-source-ls-git-buffers helm-source-ls-git))
 '(ivy-extra-directories nil t)
 '(ivy-format-function 'ivy-format-function-arrow t)
 '(ivy-height 30 t)
 '(ivy-initial-inputs-alist nil t)
 '(ivy-use-virtual-buffers t t)
 '(js-indent-level 2)
 '(js2-basic-offset 2 t)
 '(js2-bounce-indent-p t t)
 '(js2-highlight-external-variables nil t)
 '(js2-include-browser-externs nil t)
 '(js2-include-jslint-globals nil t)
 '(js2-mode-show-parse-errors nil t)
 '(js2-mode-show-strict-warnings nil t)
 '(langtool-default-language "en-US" t)
 '(langtool-java-user-arguments '("-Dfile.encoding=UTF-8") t)
 '(langtool-language-tool-jar
   "/home/derui/.emacs.d/share/LanguageTool-4.2/languagetool-commandline.jar" t)
 '(migemo-coding-system 'utf-8-unix t)
 '(migemo-command "cmigemo" t)
 '(migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict" t)
 '(migemo-options '("-q" "--emacs") t)
 '(migemo-pattern-alist-length 1024 t)
 '(migemo-regex-dictionary nil t)
 '(migemo-use-frequent-pattern-alist t t)
 '(migemo-use-pattern-alist t t)
 '(migemo-user-dictionary nil t)
 '(mozc-helper-program-name "/usr/bin/mozc_emacs_helper" t)
 '(nyan-animate-nyancat t t)
 '(ocamlformat-show-errors nil)
 '(org-adapt-indentation nil)
 '(org-agenda-current-time-string "← now")
 '(org-agenda-custom-commands
   '(("o" "At the office" tags-todo "@office"
      ((org-agenda-overriding-header "Office")
       (org-agenda-skip-function #'my:org-agenda-skip-all-sibling-but-first)))))
 '(org-agenda-time-grid
   '((daily today require-timed)
     (700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     "-" "────────────────"))
 '(org-bullets-bullet-list '("" "" "" "" "" "" "") t)
 '(org-clock-clocked-in-display 'none t)
 '(org-clock-out-remove-zero-time-clocks t t)
 '(org-indent-indentation-per-level 0 t)
 '(org-log-done t)
 '(org-outline-path-complete-in-steps nil)
 '(org-pomodoro-ask-upon-killing t t)
 '(org-pomodoro-format "%s" t)
 '(org-pomodoro-long-break-format "%s" t)
 '(org-pomodoro-short-break-format "%s" t)
 '(org-refile-use-outline-path 'file)
 '(org-return-follows-link t)
 '(org-src-fontify-natively t)
 '(org-startup-truncated t)
 '(org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
 '(package-selected-packages
   '(google-translate pyvenv dashboard ace-window imenu-list beacon projectile flycheck-posframe flycheck-postframe evil-mc treemacs-evil treemacs ivy-posframe company-quickhelp company-box which-key mozc-popup shackle terraform-mode expand-region ivy all-the-icons ivy-rich org-bullets org-clock nyan-mode doom-modeline add-node-modules-path prettier-js fish-mode auto-save-buffers-enhanced plantuml-mode request meghanada maghanada company-lsp rjsx-mode mozc ox-hugo dimmer langtool gruvbox-theme org-tree-slide ivy-hydra hydra slime-company evil-iedit-state rainbow-mode evil-cleverparens aggressive-indent aggressive-indent-mode diminish avy-migemo-e\.g\.swiper wgrep-ag symbol-overlay groovy-mode adoc-mode clj-refactor smartparens cider clojure-mode rust-mode log4e migemo avy org-pomodoro flycheck quickrun exec-path-from-shell color-theme-solarized yaml-mode web-mode ruby-end google-c-style haskell-mode ensime lua-mode markdown-mode company-go go-eldoc go-mode scss-mode stylus-mode zlc yasnippet git-gutter magit bm highlight evil-numbers evil-leader evil neotree popup popwin smartrep key-chord undo-tree recentf-ext js2-refactor js2-mode typescript-mode tide company swiper avy-migemo counsel company-mode tuareg ert-expectations auto-async-byte-compile s f wgrep ag use-package eldoc-extension caml))
 '(plantuml-options "-charset UTF-8" t)
 '(plantuml-output-type "png" t)
 '(prettier-js-show-errors nil t)
 '(projectile-completion-system 'ivy t)
 '(projectile-enable-caching t t)
 '(projectile-enable-idle-timer nil t)
 '(racer-cmd "~/.cargo/bin/racer" t)
 '(racer-rust-src-path
   "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src" t)
 '(rust-indent-offset 4 t)
 '(scss-compile-at-save nil t)
 '(shackle-default-rule '(:select t) t)
 '(shackle-rules '((compilation-mode :align t :size 0.4)) t)
 '(swiper-include-line-number-in-search t t)
 '(tuareg-function-indent 0 t)
 '(tuareg-let-always-indent t t)
 '(tuareg-match-indent 0 t)
 '(tuareg-match-patterns-aligned t t)
 '(tuareg-sig-struct-indent 0 t)
 '(typescript-indent-level 2)
 '(web-mode-code-indent-offset 2 t)
 '(web-mode-markup-indent-offset 2 t)
 '(which-key-max-description-length 40 t)
 '(which-key-use-C-h-commands t t)
 '(yas-global-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-face-highlight-read ((t (:background "gray21" :underline t))))
 '(lsp-face-highlight-write ((t (:background "gray21" :underline t))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace"))))
 '(show-paren-match ((t (:foreground nil :background nil :underline "SkyBlue" :weight bold))))
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t)))))
(put 'dired-find-alternate-file 'disabled nil)
