;;; -*- lexical-binding: t -*-
;;; configurations for packages based on leaf and leaf-keywords

(require 'leaf)

(leaf leaf-keywords
  :require t
  :config
  (leaf-keywords-init))

(leaf diminish :straight t)
(leaf hydra :straight t)

(leaf *major-mode
  :config
  (leaf *org-mode
    :config
    (leaf org
      :straight t
      :mode ("\\.org\\'" . org-mode)
      :hook (org-mode-hook . turn-on-font-lock)
      :custom (;; org-mode内部のソースを色付けする
               (org-src-fontify-natively . t)
               ;; org-modeの開始時に、行の折り返しを無効にする。
               (org-startup-truncated . t)
               ;; follow-linkから戻ることを可能とする。
               (org-return-follows-link . t)

               (org-refile-use-outline-path . 'file)
               (org-outline-path-complete-in-steps . nil)
               (org-log-done . t)
               (org-todo-keywords . '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

               (org-indent-indentation-per-level . 0)
               (org-adapt-indentation . nil)
               (org-clock-clocked-in-display . 'none)
               (org-clock-out-remove-zero-time-clocks . t))
      :config
      ;; 一時間に一回、org-modeの全てのバッファを保存する。
      (run-at-time "00:59" 3600 #'org-save-all-org-buffers)

      (leaf ob-plantuml
        :require t
        :config
        (setq org-plantuml-jar-path (expand-file-name (locate-user-emacs-file "plantuml.jar")))
        (add-to-list 'org-babel-load-languages '(plantuml . t)))

      (leaf *org-local-functions
        :config
        (defun my:org-done-todo ()
          (interactive)
          (org-todo "DONE"))

        (defun my:org-current-is-todo ()
          (string= "TODO" (org-get-todo-state)))

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
        (advice-add 'org-extract-archive-file :filter-return #'my:org-add-ymd-to-archive))

      (leaf *gtd-settings
        :if my:gtd-base-path
        :config
        (let ((inbox (expand-file-name "inbox.org" my:gtd-base-path))
              (gtd (expand-file-name "gtd.org" my:gtd-base-path))
              (someday (expand-file-name "someday.org" my:gtd-base-path))
              (tickler (expand-file-name "tickler.org" my:gtd-base-path)))
          (setq org-capture-templates
                `(("t" "Todo [inbox]" entry
                   (file+headline ,inbox "Tasks")
                   "* TODO %?")
                  ("o" "Task for office [gtd]" entry
                   (file ,gtd)
                   "* TODO %? :@office:")
                  ("h" "Task for home [gtd]" entry
                   (file ,gtd)
                   "* TODO %? :@home:")
                  ("b" "Blog idea [gtd]" entry
                   (file ,gtd)
                   "* TODO %? :@blog: \n%i%U")
                  ("T" "Tickler" entry
                   (file+headline ,tickler "Tickler")
                   "* %? \n%i%U")))

          (setq org-refile-targets `((,gtd :maxlevel . 3)
                                     (,someday :level . 1)
                                     (,tickler :maxlevel . 2))))

        (defun my:org-set-archive-name-for-month(&rest args)
          (setq-local org-archive-location (concat "./archives/"
                                                   (format-time-string "%Y%m" (current-time))
                                                   "-%s_archive::datetree/* Finished Tasks")))

        (advice-add 'org-archive-subtree :before #'my:org-set-archive-name-for-month)))

    (leaf org-agenda
      :require t
      :if my:gtd-base-path
      :custom
      (org-agenda-custom-commands .
                                  '(("o" "At the office" tags-todo "@office"
                                     ((org-agenda-overriding-header "Office")
                                      (org-agenda-skip-function #'my:org-agenda-skip-all-sibling-but-first)))))
      (org-agenda-current-time-string . "← now")
      (org-agenda-time-grid .
                            '((daily today require-timed)
                              (0700 0800 0900 01000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
                              "-"
                              "────────────────"))

      :config
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

      (let ((inbox (expand-file-name "inbox.org" my:gtd-base-path))
            (gtd (expand-file-name "gtd.org" my:gtd-base-path))
            (someday (expand-file-name "someday.org" my:gtd-base-path))
            (tickler (expand-file-name "tickler.org" my:gtd-base-path)))
        (setq org-agenda-files (list inbox gtd tickler))))

    (leaf org-clock
      :leaf-defer nil
      :require t
      :hook (org-clock-out-hook . org-update-all-dblocks)
      :custom
      (org-clock-out-remove-zero-time-clocks . t)
      (org-clock-clocked-in-display . 'frame-title)
      (org-clock-frame-title-format . '((:eval (format "%s %s"
                                                       (if (require 'org-clock-today nil t)
                                                           (if org-clock-today-count-subtree
                                                               (format "%s / %s"
                                                                       org-clock-today-subtree-time
                                                                       org-clock-today-buffer-time)
                                                             (format "%s" org-clock-today-buffer-time))
                                                         "")
                                                       org-mode-line-string))))
      :hook (kill-emacs-hook . my:org-clock-out-and-save-when-exit)
      :preface
      (defun my:org-clock-out-and-save-when-exit ()
        "Save buffers and stop clocking when kill emacs."
        (when (org-clocking-p)
          (org-clock-out)
          (save-some-buffers t)))
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
        (setq my:org-clocked-time-mode-line (my:task-clocked-time))))

    (leaf org-bullets
      :straight t
      :custom (org-bullets-bullet-list . '("" "" "" "" "" "" ""))
      :hook (org-mode-hook . org-bullets-mode))

    (leaf org-pomodoro
      :straight t
      :custom
      (org-pomodoro-ask-upon-killing . t)
      (org-pomodoro-format . "%s")
      (org-pomodoro-short-break-format . "%s")
      (org-pomodoro-long-break-format . "%s")
      :bind (:org-agenda-mode-map
             :package org-agenda
             ("P" . org-pomodoro))
      :hook
      (org-pomodoro-started-hook . my:org-add-task-time-to-mode-line)
      (org-pomodoro-finished-hook . my:org-remove-task-time-from-mode-line)
      (org-pomodoro-tick-hook . my:update-task-clocked-time)
      (org-pomodoro-started-hook . my:org-pomodoro-started-hook)
      (org-pomodoro-finished-hook . my:org-pomodoro-finished-hook)
      (org-pomodoro-short-break-finished-hook . my:org-pomodoro-short-break-hook)
      (org-pomodoro-long-break-finished-hook . my:org-pomodoro-long-break-hook)
      :preface
      (defun my:org-pomodoro-started-hook ()
        (notifications-notify
         :title "org-pomodoro"
         :body "Let's focus for 25 minutes!"))
      (defun my:org-pomodoro-finished-hook ()
        (notifications-notify
         :title "org-pomodoro"
         :body "Well done! Take a break."))
      (defun my:org-pomodoro-short-break-hook ()
        (notifications-notify
         :title "org-pomodoro"
         :body "Finish short break. Will do next round!"))
      (defun my:org-pomodoro-long-break-hook ()
        (notifications-notify
         :title "org-pomodoro"
         :body "Finish long break."))
      (defun my:org-add-task-time-to-mode-line ()
        (add-to-list 'global-mode-string 'my:org-clocked-time-mode-line t))

      (defun my:org-remove-task-time-from-mode-line ()
        (when (memq 'my:org-clocked-time-mode-line global-mode-string)
          (setq global-mode-string
                (remove 'my:org-clocked-time-mode-line global-mode-string)))))

    (leaf ox-hugo
      :straight t
      :hook
      (org-mode-hook . my:org-hugo-enable-if-hugo-buffer)
      :preface
      (defun my:org-hugo-enable-if-hugo-buffer ()
        (let ((prop (my:org-global-props "HUGO_.\+" (current-buffer))))
          (when prop
            (org-hugo-auto-export-mode +1)))))

    (leaf org-onit
      :after org-clock org
      :straight (org-onit :type git :host github :repo "takaxp/org-onit")
      :bind
      (("C-<f11>" . org-clock-goto)
       (:org-mode-map
        :package org
        ("<f11>" . org-onit-toggle-doing)
        ("S-<f11>" . org-onit-goto-anchor))))

    (leaf org-clock-today
      :straight t
      :after org-clock
      :config
      (org-clock-today-mode 1)))

  (leaf go-mode
    :straight t
    :bind (:go-mode-map
           ("M-." . godef-jump))
    :hook
    (go-mode-hook . my:go-mode-hook-1)
    :config
    (defun my:go-mode-hook-1 ()
      (add-hook 'before-save-hook #'lsp-format-buffer t t)
      (add-hook 'before-save-hook #'lsp-organize-imports t t)
      (lsp)))

  (leaf *common-lisp
    :config
    (leaf lisp-mode
      :require t
      :preface
      (defun my:lisp-hooks ()
        (setq-local company-idle-delay 0.2)
        (setq-local show-paren-style 'expression)
        (set-newline-and-indent))
      :hook
      (lisp-mode-hook . my:lisp-hooks))

    (leaf *roswell
      :if (let ((helper (expand-file-name "helper.el" my:roswell-path)))
            (and (file-exists-p helper) my:roswell-path))
      :config
      (defvar roswell-slime-contribs '(slime slime-fancy))
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
        (slime-qlot-exec directory)))

    (leaf hyperspec
      :when (featurep 'slime)
      :require t
      :custom
      ;; HyperSpecをewwで見る設定
      (common-lisp-hyperspec-root . "~/.emacs.d/share/HyperSpec/")

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
          (display-buffer "*eww*")))))

  (leaf ruby
    :config
    (leaf ruby-mode
      :require t
      :mode ("\\.rb$" . ruby-mode)
      :bind (:ruby-mode-map
             ("C-c x" . xmp)
             ("C-M-i" . rct-complete-symbol--anything)))

    (leaf ruby-end
      :straight t
      :hook (ruby-mode-hook . ruby-end-mode)))

  (leaf rust-mode
    :straight t
    :custom
    (rust-indent-offset . 4)
    (racer-rust-src-path . my:rust-src-location)
    (racer-cmd . my:rust-racer-path)
    :hook
    (rust-mode-hook . racer-mode)
    (rust-mode-hook . eldoc-mode))

  (leaf *python
    :config
    (leaf pyvenv
      :straight t
      :if (and my:virtualenv-path (file-exists-p my:virtualenv-path))
      :config
      (pyvenv-activate my:virtualenv-path))

    (leaf python
      :mode ("\\.py$" . python-mode)
      :hook
      (python-mode-hook . my:python-mode-hook-0)
      :preface
      (defun my:python-mode-hook-0 ()
        (setq-local indent-tabs-mode nil)

        (flycheck-mode +1))))

  (leaf *emacs-lisp
    :config
    (leaf elisp-mode
      :require t
      :preface
      (defun my:emacs-lisp-hooks ()
        (setq-local company-idle-delay 0.2)
        (setq-local company-backends '(company-semantic company-files company-elisp))
        (setq-local show-paren-style 'expression)
        (set-newline-and-indent))

      :hook
      (emacs-lisp-mode-hook . my:emacs-lisp-hooks)
      :config
      (leaf eldoc
        :commands eldoc-mode
        :custom
        ;; idle時にdelayをかけない
        (eldoc-idle-delay . 0)
        ;; echo areaに複数行表示を有効にする
        (eldoc-echo-area-use-multiline-p . t)
        :hook
        (emacs-lisp-mode-hook . eldoc-mode)
        (lisp-interaction-mode-hook . eldoc-mode)
        (ielm-mode-hook . eldoc-mode))))

  (leaf *ocaml
    :config

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
      (add-to-list 'load-path (my:opam-load-path)))

    (leaf ocamlformat
      :commands ocamlformat-before-save
      :custom
      (ocamlformat-show-errors . nil))

    (leaf tuareg
      :straight t
      :mode
      ("\\.ml[ily]?\\'" . tuareg-mode)
      ("\\.topml\\'" . tuareg-mode)
      :custom
      ;; Global tuareg setting
      (tuareg-let-always-indent . t)
      (tuareg-function-indent . 0)
      (tuareg-match-indent . 0)
      (tuareg-sig-struct-indent . 0)
      (tuareg-match-patterns-aligned . t)
      :hook
      (tuareg-mode-hook . tuareg-mode-hook-1)
      :bind
      (:tuareg-mode-map ("C-c C-c" . my:dune-compile))
      :preface
      (defun tuareg-mode-hook-1 ()
        (let ((bufname (buffer-name)))

          (unless (string-match "ocamlformat[a-zA-Z0-9]+?\\.mli?\\'" bufname)
            (electric-indent-mode 1)

            (when (featurep 'flyspell)
              (flyspell-prog-mode))

            (setq-local company-backends '((company-semantic company-files)))
            (add-hook 'before-save-hook #'ocamlformat-before-save nil t)
            (lsp))))

      (defun my:dune-compile ()
        (interactive)
        (save-buffer)
        (let* ((default-directory
                 (or (locate-dominating-file buffer-file-name "Makefile") default-directory))
               (compile-command (concat "(cd " default-directory " && dune build @check)"))
               (compilation-directory
                (or (locate-dominating-file buffer-file-name "Makefile") nil)))
          (recompile)))))

  (leaf adoc-mode
    :straight t
    :mode ("\\.adoc\\'" . adoc-mode))

  (leaf lua-mode
    :straight t
    :mode ("\\.lua\\'" . lua-mode))

  (leaf markdown-mode
    :straight t
    :mode ("\\.md\\'" . markdown-mode))

  (leaf rst
    :require t
    :mode ("\\.rst\\'" . rst-mode))

  (leaf css-mode
    :require t
    :mode ( ("\\.css\\'" . css-mode) ("\\.scss\\'" . scss-mode))
    :custom
    (scss-compile-at-save . nil)
    :hook
    (scss-mode-hook . my:scss-mode-hook-0)
    (css-mode-hook . my:scss-mode-hook-0)
    :preface
    (defun my:scss-mode-hook-0 ()
      (add-node-modules-path)

      (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
      (setq-local flycheck-css-stylelint-executable "stylelint")
      (setq-local prettier-js-args '("--parser" "css" "--pkg-conf"))
      (setq-local prettier-js-command "prettier")
      (setq-local company-backends '((company-semantic company-files)))
      (prettier-js-mode +1)
      (flycheck-mode +1)

      (setq-local css-indent-offset 2)
      (setq-local company-backends '(company-semantic
                                     company-files
                                     company-css))))

  (leaf yaml-mode
    :straight t
    :mode ("\\.yml\\'" . yaml-mode))

  (leaf web-mode
    :straight t
    :mode
    ("\\.html\\'" . web-mode)
    ("\\.rt\\'" . web-mode)
    :custom
    (web-mode-markup-indent-offset . 2)
    (web-mode-code-indent-offset . 2)
    :hook
    (web-mode-hook . my:web-mode-hook-enable-jsx)
    :preface
    (defun my:web-mode-hook-enable-jsx ()))

  (leaf stylus-mode
    :straight t
    :mode ("\\.styl\\'" . stylus-mode))

  (leaf clojure
    :config
    (leaf clojure-mode
      :straight t
      :after clj-refactor
      :hook
      (clojure-mode-hook . my:clojure-mode-hook-0)
      (clojure-mode-hook . smartparens-strict-mode)
      (clojure-mode-hook . clj-refactor-mode)
      :config
      (defun my:clojure-mode-hook-0 ()
        (cljr-add-keybindings-with-prefix "C-c j")))

    (leaf clj-refactor :straight t)

    (leaf cider
      :straight t
      :hook
      (cider-mode-hook . eldoc-mode)
      :custom
      (cider-repl-display-in-current-window . t)
      (cider-repl-use-clojure-font-lock . t)
      (cider-save-file-on-load . 'always-save)
      (cider-font-lock-dynamically . '(macro core function var))
      (cider-overlays-use-font-lock . t)
      :config
      (cider-repl-toggle-pretty-printing)))

  (leaf *javascript/typescript
    :config
    (leaf prettier-js
      :straight t
      :commands prettier-js-mode
      :custom
      ;; do not show error
      (prettier-js-show-errors . nil))

    (leaf add-node-modules-path :straight t)

    (leaf js2-mode
      :straight t
      :commands js2-minor-mode js2-mode
      :custom
      (js2-bounce-indent-p . nil)
      (js2-basic-offset . 2)
      (js2-include-browser-externs . nil)
      (js2-mode-show-parse-errors . nil)
      (js2-mode-show-strict-warnings . nil)
      (js2-highlight-external-variables . nil)
      (js2-include-jslint-globals . nil)
      :config
      (leaf *before-emacs-27
        :if (version< emacs-version "27.0")
        :mode
        ("\\.js\\'" . js2-mode)
        ("\\.es6\\'" . js2-mode))

      (leaf *after-emacs-27
        :if (version<= "27.0" emacs-version)
        :hook
        (js-mode-hook . js2-minor-mode)))

    (leaf js-mode
      :after flycheck
      :commands js-mode
      :custom
      (js-indent-level . 2)
      :preface
      (defun my:js-mode-hook ()
        (flycheck-mode +1))
      :hook
      (js-mode-hook . my:js-mode-hook)
      :config
      (leaf *after-emacs-27
        :if (version<= "27.0" emacs-version)
        :mode
        (( "\\.js\\'" "\\.es6\\'") . js-mode)))

    (leaf rjsx-mode
      :commands rjsx-mode
      :mode
      ("components\\/.*\\.js\\'" . rjsx-mode)
      ("containers\\/.*\\.js\\'" . rjsx-mode))

    (leaf typescript-mode
      :straight t
      :mode ("\\.tsx?\\'" . typescript-mode)
      :hook
      (typescript-mode-hook . my:typescript-mode-hook)
      :bind (:typescript-mode-map
             ("M-j" . c-indent-new-comment-line))
      :custom
      (typescript-indent-level . 2)
      :preface

      (defun my:typescript-mode-hook ()
        (add-node-modules-path)
        (lsp)

        (setq-local prettier-js-args '("--parser" "typescript" "--pkg-conf"))
        (setq-local prettier-js-command "prettier")
        (prettier-js-mode +1)

        (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
        (setq-local company-backends '((company-semantic company-files) company-capf))
        ;; use eslint after lsp's diagnostic.
        (flycheck-add-next-checker 'lsp 'javascript-eslint)
        (flycheck-mode +1))

      :config
      (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-mode 'javascript-eslint 'typescript-mode)))

  (leaf terraform-mode
    :straight t
    :mode ("\\.tf\\'" . terraform-mode))

  (leaf plantuml-mode
    :straight t
    :custom
    (plantuml-output-type . "png")
    (plantuml-options . "-charset UTF-8")
    (plantuml-default-exec-mode . 'jar)
    :config
    (let ((plantuml-jar-file (expand-file-name (locate-user-emacs-file "plantuml.jar"))))
      (setq plantuml-jar-path plantuml-jar-file)
      (unless (file-exists-p plantuml-jar-file)
        (call-process "curl" nil nil t "-L" "-o" plantuml-jar-file
                      "https://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))))

  (leaf groovy-mode
    :straight t
    :mode ("\\.groovy\\'" . groovy-mode))

  (leaf dashboard
    :straight t
    :diminish t
    :custom
    (dashboard-startup-banner . 4)
    (dashboard-items . '((recents . 15)
                         (projects . 5)
                         (agenda . 5)))
    :config
    (dashboard-setup-startup-hook)
    (let ((fname (expand-file-name "4.txt" dashboard-banners-directory)))
      (with-temp-buffer
        (insert "
██████╗ ███████╗██████╗ ██╗   ██╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
██╔══██╗██╔════╝██╔══██╗██║   ██║██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
██║  ██║█████╗  ██████╔╝██║   ██║█████╗  ██╔████╔██║███████║██║     ███████╗
██║  ██║██╔══╝  ██╔══██╗██║   ██║██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
██████╔╝███████╗██║  ██║╚██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
╚═════╝ ╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
        (write-file fname))))

  (leaf magit
    :straight t
    :hook (git-commit-mode-hook . my:flyspell-enable))

  (leaf protobuf-mode
    :straight t
    :mode ("\\.proto\\'" . protobuf-mode)
    :hook
    (protobuf-mode-hook . my:protobuf-mode-hook)
    :config
    (defconst my:protobuf-style
      '((c-basic-offset . 2)
        (indent-tabs-mode . nil)))
    (defun my:protobuf-mode-hook ()
      (c-add-style "my-protobuf-style" my:protobuf-style))))

(leaf *minor-mode
  :config

  (leaf cc-mode
    :require t
    ;; .hはc++-modeで開く
    :mode ("\\.h$" . c++-mode)
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

    :hook
    (c-mode-common-hook . my:c-mode-hook))

  (leaf ace-window
    :straight t
    :bind
    (:evil-window-map
     :package evil
     ("C-w" . ace-select-window)))
  (leaf avy :straight t)

  (leaf yasnippet
    :straight t
    :bind (:yas-minor-mode-map
           ("TAB" . nil)
           ("<tab>" . nil)
           ("<C-tab>" . yas-expand))
    :commands yas-expand yas-global-mode
    :hook (emacs-startup-hook . yas-global-mode))

  ;; Enable overlay symbol on each programming mode
  (leaf symbol-overlay
    :straight t
    :hook (prog-mode-hook . symbol-overlay-mode)
    :custom-face
    (symbol-overlay-default-face . '((t (:background "gray21" :underline t)))))

  (leaf beacon
    :straight t
    :commands beacon-mode
    :custom
    (beacon-color . "yellow")
    :hook
    (emacs-startup-hook . beacon-mode))

  (leaf imenu-list
    :straight t
    :custom
    (imenu-list-size . 0.25)
    (imenu-list-auto-resize . nil)
    (imenu-list-focus-after-activation . t))

  (leaf which-key
    :straight t
    :custom
    (which-key-max-description-length . 40)
    (which-key-use-C-h-commands . t)
    :hook
    (emacs-startup-hook . which-key-mode))

  (leaf smartparens
    :straight t
    :commands sp-local-pair smartparens-global-mode
    :hook
    (prog-mode-hook . smartparens-mode)
    :config
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-mode "`" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil))

  (leaf fish-mode
    :straight t
    :mode ("\\.fish\\'" . fish-mode))

  ;; shackleを利用する設定
  (leaf shackle
    :straight t
    :custom
    (shackle-rules . '((compilation-mode :align t :size 0.4)))
    (shackle-default-rule . '(:select t))
    :hook
    (emacs-startup-hook . shackle-mode))

  (leaf git-gutter
    :straight t
    :custom
    (git-gutter:update-hooks . '(after-save-hook after-revert-hook))

    ;; 全体でgit-gutterを有効にする
    :hook (emacs-startup-hook . global-git-gutter-mode))

  (leaf auto-save-buffers-enhanced
    :straight t
    :custom
    (auto-save-buffers-enhanced-interval . 3.0)
    :config
    (auto-save-buffers-enhanced t))

  (leaf flycheck
    :straight t
    :commands (flycheck-mode flycheck-add-mode flycheck-next-error flycheck-previous-error)
    :bind
    (("<f3>" . flycheck-next-error)
     ("S-<f3>" . flycheck-previous-error))
    :custom
    (flycheck-javascript-eslint-executable . "eslint")
    :config
    (advice-add 'flycheck-eslint-config-exists-p :override (lambda () t))
    :hydra
    (hydra-flycheck nil
                    "
      Navigate Error^^    Miscellaneous
      ---------------------------------------------------
      [_k_] Prev          [_c_] Clear
      [_j_] Next
      [_f_] First Error   [_q_] Quit
      [_l_] Lask Error
      "
                    ("j" flycheck-next-error)
                    ("k" flycheck-previous-error)
                    ("f" flycheck-first-error)
                    ("l" (progn (goto-char (point-max)) (fiycheck-previous-error)))
                    ("c" flycheck-clear)
                    ("q" nil)))

  (leaf posframe
    :straight t
    :when (and window-system my:use-posframe)
    :config

    (when (eq (window-system) 'x)
      (setq posframe-gtk-resize-child-frames 'resize-mode))

    (leaf mozc-posframe
      :straight (mozc-posframe :type git :host github :repo "derui/mozc-posframe")
      :if (and my:use-mozc-el my:mozc-helper-locate)
      :config
      (mozc-posframe-register))

    (leaf flycheck-posframe
      :after flycheck
      :straight t
      :hook (flycheck-mode-hook . flycheck-posframe-mode))

    ;; using child frame
    (leaf company-posframe
      :after company
      :straight t
      :hook (company-mode-hook . company-posframe-mode))

    (leaf ivy-posframe
      :after ivy
      :straight t
      :custom
      (ivy-posframe-parameters . '((left-fringe . 8)
                                   (right-fringe . 8)
                                   (internal-border-width . 1)))
      (ivy-posframe-display-functions-alist . '((swiper . nil)
                                                (t      . ivy-posframe-display-at-frame-center)))
      :config
      (ivy-posframe-mode 1)))

  (leaf general
    :straight t
    :after evil org
    :preface
    (defconst my:general:leader-key "SPC")
    :config
    (general-create-definer my:leader-def
      :prefix my:general:leader-key)
    (my:leader-def
      :keymaps 'evil-normal-state-map
      ";" 'ivy-switch-buffer-other-window
      "p" 'projectile-command-map
      "r" 'google-translate-smooth-translate
      "hf" 'hydra-flycheck/body
      "i" 'hydra-evil-mc/body
      "q" 'evil-delete-buffer
      "w" 'save-buffer
      ;; for org-mode
      "oc" 'org-capture
      "od" 'my:org-done-todo
      "os" 'org-toggle-narrow-to-subtree
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
      "cf" 'counsel-git
      "ca" 'counsel-apropos
      "x" 'counsel-M-x))

  (leaf evil
    :straight t
    :hook
    (emacs-startup-hook . evil-mode)
    :bind
    (:evil-normal-state-map
     ("TAB" . nil)
     ("s" . evil-avy-goto-char)
     ("K" . lsp-ui-doc-glance))
    ;; evil-jump-forwardを潰す。
    (:evil-motion-state-map
     ("TAB" . nil))
    (:evil-visual-state-map
     ("f" . evil-avy-goto-char))
    :preface

    (defun my:evil-swap-key (map key1 key2)
      ;; MAP中のKEY1とKEY2を入れ替え
      "Swap KEY1 and KEY2 in MAP."
      (let ((def1 (lookup-key map key1))
            (def2 (lookup-key map key2)))
        (define-key map key1 def2)
        (define-key map key2 def1)))

    :config
    (defun my:avy-goto-line-below-same-column ()
      (interactive)
      (let ((col (current-column)))
        (avy-goto-line-below)
        (move-to-column col)))
    (declare-function 'my:avy-goto-line-below-same-column "avy")
    (evil-define-avy-motion my:avy-goto-line-below-same-column inclusive)

    (defun my:avy-goto-line-above-same-column ()
      (interactive)
      (let ((col (current-column)))
        (avy-goto-line-above)
        (move-to-column col)))
    (declare-function 'my:avy-goto-line-above-same-column "avy")
    (evil-define-avy-motion my:avy-goto-line-above-same-column inclusive)

    (evil-define-key nil evil-visual-state-map "J" #'evil-my:avy-goto-line-below-same-column)
    (evil-define-key nil evil-visual-state-map "K" #'evil-my:avy-goto-line-above-same-column)

    (evil-set-initial-state 'dashboard-mode 'emacs)
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

    (evil-ex-define-cmd "eval" 'eval-expression)
    (evil-ex-define-cmd "ev" "eval")

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

    ;; https://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
    (evil-define-command my:maybe-exit()
      :repeat change
      (interactive)
      (let ((modified (buffer-modified-p)))
        (insert "k")
        (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                               nil 0.2)))
          (cond
           ((null evt) (message ""))
           ((and (integerp evt) (char-equal evt ?j))
            (delete-char -1)
            (set-buffer-modified-p modified)
            (push 'escape unread-command-events))
           (t (setq unread-command-events (append unread-command-events (list evt))))))))

    (leaf *key-bindings
      :after ivy counsel
      :config
      (setcdr evil-insert-state-map nil)

      (define-key evil-insert-state-map "k" #'my:maybe-exit)
      (define-key evil-insert-state-map [escape] #'evil-normal-state)

      (when (and my:use-mozc-el my:mozc-helper-locate)
        (progn

          (defun my:evil-change-input-method (ime-state)
            (let ((when-emacs-state (string= evil-state "emacs")))
              (cond
               ((and ime-state (or (not current-input-method) (string-equal current-input-method my:input-method)))
                ;; TODO: work around to avoid invalid mozc input behavior
                (define-key evil-insert-state-map "k" nil)
                (set-input-method my:input-method)
                (when (evil-normal-state-p)
                  (evil-insert-state)))
               (t
                ;; TODO: work around to avoid invalid mozc input behavior
                (define-key evil-insert-state-map "k" #'my:maybe-exit)
                (set-input-method nil)))))

          (defun my:evil-enable-ime ()
            (interactive)
            (my:evil-change-input-method t))

          (defun my:evil-disable-ime ()
            (interactive)
            (my:evil-change-input-method nil))

          (define-key evil-insert-state-map (kbd "<Hangul>") #'my:evil-enable-ime)
          (define-key evil-insert-state-map (kbd "<henkan>") #'my:evil-enable-ime)
          (define-key evil-insert-state-map (kbd "<Hangul_Hanja>") #'my:evil-disable-ime)
          (define-key evil-insert-state-map (kbd "<muhenkan>") #'my:evil-disable-ime)))))

  (leaf evil-cleverparens
    :straight t
    :after elisp-mode lisp-mode
    :hook
    ((emacs-lisp-mode-hook lisp-mode-hook) . evil-cleverparens-mode))

  (leaf evil-mc
    :straight t
    :hook (emacs-startup-hook . global-evil-mc-mode)
    :hydra
    (hydra-evil-mc nil
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

  (leaf evil-numbers
    :straight t
    :commands evil-numbers/dec-at-pt evil-numbers/inc-at-pt
    :bind
    (:evil-normal-state-map
     :package evil
     ("C-a" . evil-numbers/inc-at-pt)
     ("C-x" . evil-numbers/dec-at-pt)))

  (leaf evil-surround
    :straight t
    :hook (emacs-startup-hook . global-evil-surround-mode))

  (leaf lsp-mode
    ;; temporary workaround. https://github.com/emacs-lsp/lsp-mode/pull/2109
    :straight (lsp-mode :repo "emacs-lsp/lsp-mode"
                        :fetcher github
                        :files (:defaults
                                "clients/*.el"))
    :custom
    ;; debug
    (lsp-print-io . nil)
    (lsp-trace . nil)
    (lsp-print-performance . nil)
    ;; general
    (lsp-auto-guess-root . t)
    (lsp-prefer-capf . t)
    ;; do not use flymake
    (lsp-enable-completion-at-point .t)
    (lsp-prefer-flymake . nil)
    (lsp-document-sync-method . 2) ;; always send incremental document
    (lsp-response-timeout . 5)
    (lsp-enable-indentation . nil)
    (lsp-enable-which-key-integration . t)
    ;; do not show signature auto activate, this help to avoid flicker of minibuffer...
    (lsp-signature-auto-activate . nil)
    :custom-face
    (lsp-face-highlight-read . '((t (:background "gray21" :underline t))))
    (lsp-face-highlight-write . '((t (:background "gray21" :underline t))))
    :bind
    (:lsp-mode-map
     ("C-c r" . lsp-rename))
    :preface
    (defun my:lsp-disable-eldoc-when-hover ()
      (when (my:minor-mode-active-p 'lsp-mode)
        (setq-local eldoc-message-function (lambda (&rest _) (progn)))))

    (defun my:lsp-disable-symbol-overlay ()
      (symbol-overlay-mode -1))

    (setq lsp-keymap-prefix "C-c C-l")
    :hook
    (python-mode-hook . lsp)

    (lsp-mode-hook . my:lsp-disable-eldoc-when-hover)
    (lsp-mode-hook . my:lsp-disable-symbol-overlay)
    :config
    ;; use lsp-mode's implemented capf integration
    (setq lsp-enable-completion-at-point (not my:use-company-lsp))
    (setq lsp-prefer-capf (not my:use-company-lsp)))

  (leaf lsp-treemacs :straight t :after lsp-mode)

  (leaf lsp-ui
    :straight t
    :after lsp-mode
    :commands lsp-ui-doc-show
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable . nil)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-position . 'at-point) ;; top, bottom, or at-point
    (lsp-ui-doc-alignment . 'window) ;; top-right of the frame
    (lsp-ui-doc-max-width . 150)
    (lsp-ui-doc-max-height . 30)
    (lsp-ui-doc-use-childframe . t)
    (lsp-ui-doc-use-webkit . nil)
    ;; lsp-ui-flycheck
    ;; use flycheck on the fly
    (lsp-ui-flycheck-enable . nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable . nil)
    (lsp-ui-sideline-ignore-duplicate . t)
    (lsp-ui-sideline-show-symbol . t)
    (lsp-ui-sideline-show-hover . t)
    (lsp-ui-sideline-show-diagnostics . nil)
    (lsp-ui-sideline-show-code-actions . nil)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable . nil)
    (lsp-ui-imenu-kind-position . 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable . t)
    (lsp-ui-peek-peek-height . 20)
    (lsp-ui-peek-list-width . 50)
    (lsp-ui-peek-fontify . 'always) ;; never, on-demand, or always
    :preface
    (defun my:toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:lsp-mode-map
     :package lsp-mode
     ("C-c C-r" . lsp-ui-peek-find-references)
     ("C-c C-j" . lsp-ui-peek-find-definitions)
     ("C-c i"   . lsp-ui-peek-find-implementation)
     ("C-c m"   . imenu-list-smart-toggle)
     ("C-c s"   . lsp-ui-sideline-mode)
     ("C-c d"   . my:toggle-lsp-ui-doc))
    :hook
    (lsp-mode-hook . lsp-ui-mode)
    :config
    (ad-disable-regexp "lsp-ui-doc-.+")
    (ad-activate 'select-window))

  (leaf aggressive-indent
    :straight t
    :commands aggressive-indent-mode
    :hook
    (lisp-mode-hook . aggressive-indent-mode)
    (emacs-lisp-mode-hook . aggressive-indent-mode)))

(leaf *utility-package
  :config

  (leaf all-the-icons
    :straight t
    :custom
    (all-the-icons-scale-factor . 1.0))

  (leaf notifications :require t)
  (leaf s
    :straight t
    :commands s-join)

  (leaf exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize)
    (let ((envs '("GOROOT" "GOPATH" "PATH")))
      (exec-path-from-shell-copy-envs envs)))

  (leaf google-translate
    :straight t
    :custom
    (google-translate-translation-directions-alist . '(("ja" . "en") ("en" . "ja")))
    :config ;; Workaround for search failed. See https://github.com/atykhonov/google-translate/issues/52#issuecomment-481310626
    (with-eval-after-load "google-translate-tk"
      (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

    (leaf google-translate-smooth-ui :require t))

  (leaf ripgrep :straight t)
  (leaf ag :straight t
    :config
    (leaf wgrep-ag
      :straight t
      :bind (:ag-mode-map
             :package ag
             ("r" . wgrep-change-to-wgrep-mode))
      :hook (ag-mode-hook . wgrep-ag-setup)))

  (leaf langtool
    :straight t
    :commands langtool-details-error-message
    :custom
    (langtool-language-tool-jar . my:langtool-cli-path)
    (langtool-default-language . "en-US")
    (langtool-java-user-arguments . '("-Dfile.encoding=UTF-8"))))

(leaf *company-packages
  :config
  (leaf company
    :straight t
    :diminish t
    :custom
    (company-dabbrev-downcase . nil)
    (company-idle-delay . 0)
    ;; 2文字入力で補完されるように
    (company-minimum-prefix-length . 2)
    ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
    (company-selection-wrap-around . t)
    (company-tooltip-align-annotations . t)
    :bind
    (:company-active-map
     ("M-n" . nil)
     ("M-p" . nil)
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous)
     ("C-s" . company-filter-candidates)
     ("C-h" . nil)
     ("TAB" . company-complete-common)
     ;; ドキュメント表示
     ("M-d" . company-show-doc-buffer))
    ;; C-n, C-pで補完候補を選べるように
    (:company-search-map
     ("C-n" . company-select-next)
     ("C-p" . company-select-previous))
    :hook
    (emacs-startup-hook . global-company-mode)
    :config
    (leaf company-quickhelp
      :straight t
      :custom
      (company-quickhelp-color-foreground . "black")
      :bind (:company-active-map
             :package company
             ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode-hook . company-quickhelp-mode))

    (leaf company-box
      :straight t
      :after all-the-icons
      :hook
      (company-mode-hook . company-box-mode)
      (global-company-mode-hook . company-box-mode)
      :custom
      (company-box-doc-enable . t)
      (company-box-doc-delay . 1.0)
      (company-box-show-single-candidate . t)
      (company-box-max-candidates . 50)
      (company-box-icons-alist . 'company-box-icons-all-the-icons)
      :config
      (setq company-box-backends-colors nil)

      ;; great configuration for company-box with all-the-icons
      ;; https://ladicle.com/post/config/#company
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-fileicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
              (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
              (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
              (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
              (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
              (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
              (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
              (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
              (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
              (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))

  (leaf company-css
    :require t
    :commands company-css))

(leaf treemacs
  :straight t
  :custom
  (treemacs-is-never-other-window . t)
  :hook
  (treemacs-mode-hook . (lambda ()
                          (when (version<= "27.0.0" emacs-version)
                            (display-line-numbers-mode -1))))
  :config
  (leaf treemacs-evil :straight t :require t))

;; mozc
(leaf mozc
  :straight t
  :if (and my:use-mozc-el my:mozc-helper-locate)
  :custom
  (mozc-keymap-kana . mozc-keymap-kana-101us)
  (mozc-candidate-style . 'posframe)
  (mozc-helper-program-name . my:mozc-helper-locate))

(leaf projectile
  :straight t
  :commands projectile-register-project-type
  :hook
  (emacs-startup-hook . projectile-mode)
  :bind
  (:projectile-command-map
   ("s" . my:projectile-search-dwim))
  :custom
  (projectile-enable-idle-timer . nil)
  (projectile-enable-caching . t)
  (projectile-completion-system . 'ivy)
  :preface
  (defun my:projectile-search-dwim (search-term)
    "Merge version to search document via grep/ag/rg.
      Use fast alternative if it exists, fallback grep if no alternatives in system.
      "
    (interactive (list (projectile--read-search-string-with-default
                        "Dwim search for")))
    (cond
     ((executable-find "rg") (projectile-ripgrep search-term))
     ((executable-find "ag") (projectile-ag search-term))
     (t (projectile-grep search-term))))

  :config
  (projectile-register-project-type
   'yarn
   '("package.json")
   :compile "yarn build"
   :test "yarn test"
   :run "yarn start"
   :test-suffix ".test"))

(leaf *mode-lines
  :config
  (leaf nyan-mode
    :straight t
    :custom
    (nyan-animate-nyancat . t)
    :hook
    (emacs-startup-hook . nyan-mode))

  (leaf doom-modeline
    :straight t
    :commands doom-modeline-def-modeline
    :custom
    (doom-modeline-buffer-file-name-style . 'truncate-with-project)
    (doom-modeline-icon . t)
    (doom-modeline-major-mode-icon . t)
    (doom-modeline-minor-modes . nil)
    :hook
    (emacs-startup-hook . doom-modeline-mode)
    :config
    (doom-modeline-def-modeline
      'main
      '(bar window-number modals matches buffer-info remote-host buffer-position selection-info)
      '(misc-info debug minor-modes input-method lsp major-mode process vcs checker)))

  (leaf hide-mode-line
    :straight t
    :hook
    (imenu-list-major-mode-hook . hide-mode-line-mode)))

(leaf *interactive-search
  :config

  (leaf ivy
    :straight t
    :diminish t
    :custom
    (ivy-format-function . 'ivy-format-function-arrow)
    (ivy-use-virtual-buffers . t)
    (enable-recursive-minibuffers . t)
    (ivy-height . 30)
    (ivy-extra-directories . nil)
    (ivy-initial-inputs-alist . nil)
    :global-minor-mode t
    :bind
    (:evil-normal-state-map
     :package evil
     (";" . ivy-switch-buffer))
    :config
    (leaf ivy-hydra :straight t)
    (leaf amx :straight t)
    (leaf counsel
      :straight t
      :bind
      ("M-x" . counsel-M-x)
      ("C-x C-f" . counsel-find-file)
      (:evil-normal-state-map
       :package evil
       ("M-y" . counsel-yank-pop))
      :custom
      (counsel-yank-pop-separator . "\n-------\n")
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

    (leaf swiper
      :straight t
      :bind ("C-s" . swiper)
      :custom
      (swiper-include-line-number-in-search . t)))

  (leaf prescient
    :straight t
    :doc "Better sorting and filtering"
    :req "emacs-25.1"
    :tag "extensions" "emacs>=25.1"
    :url "https://github.com/raxod502/prescient.el"
    :emacs>= 25.1
    :leaf-defer nil
    :commands (prescient-persist-mode)
    :custom `((prescient-aggressive-file-save . t)
              (prescient-save-file . ,(locate-user-emacs-file "prescient")))
    :global-minor-mode prescient-persist-mode)

  (leaf ivy-prescient
    :straight t
    :doc "prescient.el + Ivy"
    :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
    :tag "extensions" "emacs>=25.1"
    :url "https://github.com/raxod502/prescient.el"
    :emacs>= 25.1
    :after prescient ivy
    :custom ((ivy-prescient-retain-classic-highlighting . t))
    :global-minor-mode t)

  (leaf ivy-rich
    :straight t
    :after ivy
    :global-minor-mode t
    :config
    (defun ivy-rich-switch-buffer-icon (candidate)
      (with-current-buffer
          (get-buffer candidate)
        (let ((icon (all-the-icons-icon-for-mode major-mode)))
          (if (symbolp icon)
              (all-the-icons-icon-for-mode 'fundamental-mode)
            icon))))

    (let ((rich-transformer-config '(:columns
                                     ((ivy-rich-switch-buffer-icon :width 2)
                                      (ivy-rich-candidate (:width 30))
                                      (ivy-rich-switch-buffer-size (:width 7))
                                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                                     :predicate
                                     (lambda (cand) (get-buffer cand)))))
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer
                 rich-transformer-config)
      (plist-put ivy-rich-display-transformers-list
                 'ivy-switch-buffer-other-window
                 rich-transformer-config))))

(leaf *migemo-family
  :if (and my:migemo-command
           my:migemo-dictionary
           (executable-find my:migemo-command))
  :config
  (leaf migemo
    :straight t
    :commands migemo-init
    :custom
    (migemo-command . my:migemo-command)
    (migemo-options . '("-q" "--emacs"))
    (migemo-dictionary . my:migemo-dictionary)
    (migemo-user-dictionary . nil)
    (migemo-regex-dictionary . nil)
    (migemo-coding-system 'utf-8-unix)
    ;; 遅いのを防ぐためにキャッシュする。
    (migemo-use-pattern-alist . t)
    (migemo-use-frequent-pattern-alist . t)
    (migemo-pattern-alist-length . 1024)
    :config
    (migemo-init))

  (leaf avy-migemo
    :after swiper
    :straight (avy-migemo :type git :host github :repo "tam17aki/avy-migemo")
    :config
    (avy-migemo-mode 1)
    (leaf avy-migemo-e.g.swiper :require t)))

(leaf gruvbox-theme
  :after company-box
  :straight t
  :require t
  :config
  (load-theme 'gruvbox-dark-hard t)
  (my:theme-initialize))
