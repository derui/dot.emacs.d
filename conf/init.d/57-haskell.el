;; settings for `haskell-mode', and tiny tips and tricks defines here.

(el-get 'sync '(haskell-mode
                ghc-mod
                ac-ghc-mod
                ))
(require 'haskell-mode)
(require 'ghc-mod)
(require 'ac-ghc-mod)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

(define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
(define-key haskell-mode-map (kbd "C-c C-h") '(lambda () (interactive) (helm helm-c-source-href)))
(define-key haskell-mode-map (kbd "C-x C-d") nil)
(setq haskell-hoogle-command "hoogle")

;; hrefをanything経由で引くための設定。
(defun href (kw)
  (interactive "shref: ")
  (with-current-buffer (get-buffer-create (concat "*href:" kw "*"))
    (set-buffer-file-coding-system 'utf-8)
    (when (zerop (buffer-size))
      (let ((kws (split-string kw)))
        (call-process-shell-command "href" nil t nil
                                    (car kws)
                                    (concat (cadr kws) ""))))
    (setq minibuffer-scroll-window (get-buffer-window (current-buffer) t))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(setq helm-c-source-href
      (helm-c-source-static-escript
       'helm-c-href-candidates "href"
       "~/.emacs.d/href.e"
       '(delayed)
       '(requires-pattern . 3)))

;======================================================================
; flymake-mode for haskell
;======================================================================
(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defvar multiline-flymake-mode nil)
(defvar flymake-split-output-multiline nil)
(defadvice flymake-split-output
  (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
    ad-do-it))
(defadvice flymake-split-string
  (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))

(defun haskell-init-flymake-initialize ()
  (set (make-local-variable 'multiline-flymake-mode) t)
  (if (not (null buffer-file-name)) (flymake-mode))
  )

;; flymakeをghc-flymake無しで実現するために必要。
(add-to-list 'flymake-allowed-file-name-masks
             '(".+\\.l?hsc?$" flymake-Haskell-init
               flymake-simple-cleanup flymake-get-real-file-name))
(add-to-list 'flymake-err-line-patterns
             '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
               1 2 3 4))
(add-to-list 'flymake-err-line-patterns
             '("^\\(.*\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):[ ]*\\(.+\\)"
               1 2 3 4))

;; cabalが利用可能である場合には、cabalを利用してテストを実行する
(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "flycheck_haskell.pl" (list source base-dir)))

;; this gets called by outline to deteremine the level. Just use the length of the whitespace
(defun hsk-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

;; folding for all rows, starting on the current column
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(global-set-key (kbd "C-x $") 'toggle-selective-display)

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

;; (global-set-key (kbd "C-@") 'toggle-hiding)

;; https://github.com/m2ym/auto-complete
(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod)))
(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod))))

;; `haskell-mode` initialization function add to hook and
;; define initialization function
(add-hook 'haskell-mode-hook 'haskell-mode-hook-1)
(defun haskell-mode-hook-1 ()
  (turn-on-haskell-indentation)
  (offside-trap-haskell-indentation-mode)

  ;; haskell-mode利用時のflymake関係の初期化
  (haskell-init-flymake-initialize)

  (turn-on-haskell-decl-scan)
  (ghc-init)
  (my-haskell-ac-init)

  ;; smartchrの代わりにkey-comboを利用する
  (when (featurep 'key-combo)
    (key-combo-define haskell-mode-map (kbd "-") '("-" " -> " "--"))
    (key-combo-define haskell-mode-map (kbd ">") '(">" " >> " " >>= "))
    (key-combo-define haskell-mode-map (kbd "<") '("<" " <- " " =<< "))
    (key-chord-define haskell-mode-map (kbd "]]") 'haskell-underscore-to-camelcase)
    (key-combo-mode 1))

  ;; haskellで利用するようなカッコなどについては、自動挿入されるようにする。
  (parenthesis-register-keys "('\"[" haskell-mode-map)

  (auto-complete-mode t)

  ;; snippetを有効にする。
  (yas/minor-mode-on)

  ;; outline uses this regexp to find headers. I match lines with no indent and indented
  ;; some lines, such as "--" ... "class"
  (setq outline-regexp "^[^\t ].*\\|^.*[\t ]+\\(where\\|of\\|do\\|in\\|if\\|then\\|else\\|let\\|module\\|import\\|deriving\\|instance\\|class\\)[\t\n ]")
  ;; enable our level computation
  (setq outline-level 'hsk-outline-level)
  (outline-minor-mode t)
  )

;; haskellのドキュメントをhelmで選択するための処理
(when (featurep 'helm)
  (defvar helm-c-source-ghc-mod
    '((name . "ghc-browse-document")
      (init . helm-c-source-ghc-mod)
      (candidates-in-buffer)
      (candidate-number-limit . 9999999)
      (action ("Open" . helm-c-source-ghc-mod-action))))

  (defun helm-c-source-ghc-mod ()
    (unless (executable-find "ghc-mod")
      (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
    (let ((buffer (helm-candidate-buffer 'global)))
      (with-current-buffer buffer
        (call-process "ghc-mod" nil t t "list"))))

  (defun helm-c-source-ghc-mod-action (candidate)
    (interactive "P")
    (let* ((pkg (ghc-resolve-package-name candidate)))
      (helm-aif (and pkg candidate)
          (ghc-display-document pkg it nil)
        (message "No document found"))))

  (defun helm-ghc-browse-document ()
    (interactive)
    (helm helm-c-source-ghc-mod))

  ;; M-x helm-ghc-browse-document() に対応するキーの割り当て
  ;; ghc-mod の設定のあとに書いた方がよいかもしれません
  (add-hook 'haskell-mode-hook
            (lambda()
              (define-key haskell-mode-map (kbd "C-M-d") 'helm-ghc-browse-document)))
  )
