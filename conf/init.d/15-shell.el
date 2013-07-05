;; eshellも含めた、各種shellについての設定を記述する

;; 実行するShellはzsh
(setq explicit-shell-file-name "/bin/zsh")
(setq shell-file-name "/bin/zsh")
(setq shell-command-switch "-c")
(setenv "PATH" (concat (expand-file-name "~/bin:") (getenv "PATH")))
(setenv "EMACS" "t")

;; ansi-colorでエスケープシーケンスをfontifyする設定
;; http://d.hatena.ne.jp/rubikitch/20081102/1225601754
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; zsh からコマンドラインを編集する際に実行する。
(defun zsh-edit-command-line-hook ()
  (when (string-match "^zshecl" (buffer-name))
   ; (skk-mode 1)
    ))
(add-hook 'find-file-hook 'zsh-edit-command-line-hook)

;; (@* "eshellに関係する設定を記述する")
(req eshell)
;;; 処理を1000行以内で切り詰める
(defvar eshell-truncate-cycle 100)
(defvar eshell-truncate-count 0)

;; eshell におけるパスを設定する
(setq eshell-path-env (getenv "PATH"))

;; 1000 行で切り詰める
(setq eshell-buffer-maximum-lines 1000)
(add-hook 'eshell-output-filter-functions
          (lambda ()
            (setq eshell-truncate-count (1+ eshell-truncate-count))
            (when (> eshell-truncate-count eshell-truncate-cycle)
              (eshell-truncate-buffer)
              (setq eshell-truncate-count 0))))

;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))

;; ヒストリのサイズ
(setq eshell-history-size 10000)
;; 補完時にサイクルする
;(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-completions nil)
;;補完候補がこの数値以下だとサイクルせずに候補表示
;(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)
;; visualなコマンドに対するTERM環境変数を設定する。
(setq eshell-term-name "ansi")

;; prompt 文字列の変更
(req vc-git)
(setq eshell-prompt-function
      (lambda ()
        (concat
         "["
         (user-login-name) "@" (system-name) " "
         (eshell/pwd)
         "(" (vc-git-mode-line-string (eshell/pwd)) ")"
         "]\n"
         (if (= (user-uid) 0)
             "#"
           "$")
         " "
         )))

(setq eshell-prompt-regexp "^[^$#\n]*[$#] ")

(req pcomplete)
;; sudoのあとも補完可能に
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-help "complete after sudo"))
    (pcomplete-here (pcomplete-here (eshell-complete-commands-list)))))

;; sudo時にtrampを経由しないように。
(fmakunbound 'eshell/sudo)
(setq eshell-password-prompt-regexp "\\(パスワード\\|[Pp]ass\\(word\\|phrase\\)\\).*:\\s *\\'")

;; ansi-termを利用するようなコマンドに対してはhookでかける。
;; (remove-hook 'eshell-first-time-mode-hook
;;           (lambda () (append-to-list eshell-visual-commands
;;                                      '("sudo"))))

;; トグルする設定
(defun my:toggle-term ()
  "eshell と直前のバッファを行き来する。C-u 付きで呼ぶと 今いるバッファと同じディレクトリに cd して開く"
  (interactive)
  (let ((ignore-list '("*Help*" "*Minibuf-1*" "*Messages*" "*Completions*"
                       "*terminal<1>*" "*terminal<2>*" "*terminal<3>*"))
        (dir default-directory))
    (labels
        ((_my:toggle-term (target)
           (if (null (member (buffer-name (second target)) ignore-list))
               (if (equal "*eshell*" (buffer-name (window-buffer)))
                   (switch-to-buffer (second target))
                 (multi-eshell-switch)
                 ;; (switch-to-buffer "*eshell*")
                 (when current-prefix-arg
                   (cd dir)
                   (eshell-interactive-print (concat "cd " dir "\n"))
                   (eshell-emit-prompt)))
             (_my:toggle-term (cdr target)))))
      (_my:toggle-term (buffer-list)))))
(global-set-key (kbd "C-t") 'my:toggle-term)

;; eshell での補完に auto-complete.el を使う
(require 'pcomplete)
(add-to-list 'ac-modes 'eshell-mode)
(ac-define-source pcomplete
  '((candidates . pcomplete-completions)))
(defun my:ac-eshell-mode ()
  (setq ac-sources
        '(ac-source-pcomplete
          ac-source-words-in-buffer
          ac-source-dictionary)))
(add-hook 'eshell-mode-hook 'my:eshell-config-1)
(defun my:eshell-config-1 ()
  (my:ac-eshell-mode)
  (key-combo-mode 0)
  (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
  ;; キーバインドの変更
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line))

;; エスケープシーケンスを処理
;; http://d.hatena.ne.jp/hiboma/20061031/1162277851
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'eshell-mode-hook 'ansi-color-for-comint-mode-on)

;; http://www.emacswiki.org/emacs-ja/EshellColor
(req ansi-color)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

;; eshellに対するaliasを追加する。
(eval-after-load "em-alias"
  '(progn (eshell/alias "ll" "ls -lh $*")
          (eshell/alias "la" "ls -a $*")
          (eshell/alias "lla" "ls -alh $*")
          (eshell/alias "sudo" "~/bin/sudo.sh $*")))

;; shell-popでeshellを利用するようにする。
(req shell-pop
  (setq shell-pop-window-height 25) ; percentage for shell-buffer window height
  (define-key global-map [zenkaku-hankaku] 'shell-pop)
  (shell-pop-set-internal-mode "eshell"))

;; 複数のeshellを楽に管理する。
(req multi-eshell
     ;; multi-eshellで開くのはeshell tとする。
     (setq multi-eshell-shell-function '(eshell t))
     (setq multi-eshell-name "*eshell*"))
