;;; -*- lexical-binding: t -*-
;; user-defined functions that are outer package independent

(require 'cl-lib)
(require 'seq)

;; 現在のバッファリスト名を取得する。
(defun my:buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(defun my:delete-trailing-whitespace ()
  (unless (seq-some (lambda (x) (eq major-mode x)) my:trailing-whitespace-exclude-modes)
    (delete-trailing-whitespace)))

(add-hook 'my:save-buffer-hook #'my:delete-trailing-whitespace)

(defun my:after-save-hook ()
  (run-hooks 'my:save-buffer-hook))

;; hookを実行するようにする。
(add-hook 'after-save-hook #'my:after-save-hook)

;; 実行したモードにおいて、常に自動でインデントを行うようにする。
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun my:minor-mode-active-p (mode)
  "return specified minor mode is active or not"
  (let ((active-modes (cl-remove-if-not (lambda (it) (and (boundp it) (symbol-value it))) minor-mode-list)))
    (member mode active-modes)))

;; kill-regionにおいて、リージョンが選択されていない場合には
;; backward-kill-wardを実行するように。
(defun my:kill-word-or-kill-region (f &rest args)
  (if (and (called-interactively-p 'interactive) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    (apply f args)))

(advice-add 'kill-region :around 'my:kill-word-or-kill-region)

;; kill-lineの際に、次の行の行頭に連続している空白を削除する
(defun my:kill-line-and-fixup (f &rest args)
  (if (and (not (bolp)) (eolp))
      (progn
        (forward-char)
        (fixup-whitespace)
        (backward-char))
    (apply f args)))

(advice-add 'kill-line :around 'my:kill-line-and-fixup)

(defun my:kill-word-at-point ()
  "delete word at under cursor. If spaces was under the cursor, delete horizontal spaces"
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;; (@* "root所有のファイルを開く際に、sudoで開き直すか聞く")
(defun my:file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

;; trampで開いたファイルについては、バッファ名を変更する
(defun my:th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

;; sudo でファイルを開く
(defun my:th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; find-file-hookの設定
(add-hook 'find-file-hook #'my:th-rename-tramp-buffer)

;; (@> "*scratch*をkillできないようにする")
(defun my:make-scratch (&optional arg)
  (interactive)
  ;; "*scratch*" を作成して buffer-list に放り込む
  (set-buffer (get-buffer-create "*scratch*"))
  (funcall initial-major-mode)
  (erase-buffer)
  (when (and initial-scratch-message (not inhibit-startup-message))
    (insert initial-scratch-message))
  (or arg (progn (setq arg 0)
                 (switch-to-buffer "*scratch*")))
  (cond ((= arg 0) (message "*scratch* is cleared up."))
        ((= arg 1) (message "another *scratch* is created"))))

(defun my:clear-scratch-when-kill-buffer ()
  (if (string= "*scratch*" (buffer-name))
      (progn (my:make-scratch 0) nil)
    t))

;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
(add-hook 'kill-buffer-query-functions #'my:clear-scratch-when-kill-buffer)

;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
(defun my:make-scratch-when-scratch-buffer ()
  (unless (member "*scratch*" (my:buffer-name-list))
    (my:make-scratch 1)))

(add-hook 'my:after-save-hook #'my:make-scratch-when-scratch-buffer)

;; テーマについての設定
(setq my:custom:current-theme 'gruvbox-dark-hard)
(defcustom my:custom:current-theme 'gruvbox-dark-hard "current theme for me"
  :group 'my
  :type 'symbol)
(defun my:theme-initialize ()
  (enable-theme my:custom:current-theme))

;; (@* "tmuxに依存した各種ユーティリティ関数")

;; tmuxのパス。存在しない場合は/usr/binにあるものとする。
(setq my:tmux-bin-path (if (executable-find "tmux") "tmux" "/usr/bin/tmux"))

(defun my:get-tmux-exec-command (cmd)
  "Return command-string of tmux"
  (format "%s %s" my:tmux-bin-path cmd))

(defun my:tmux-save-buffer (data &optional e)
  "Set data to buffer of tmux (use `tmux set-buffer')"
  (interactive "ssave to tmux buffer: ")
  (when data
    (call-process-shell-command
     (my:get-tmux-exec-command (format "set-buffer \"%s\"" data))  nil nil t)
    (when e
      (message (format "set %s to buffer of tmux" data)))))

(defun my:tmux-get-buffer ()
  "Get data from current buffer fo tmux, and set to top of kill-ring"
  (interactive "*")
  (let ((buffer (get-buffer-create " *tmux-output*")))
    (when buffer
      (call-process-shell-command (my:get-tmux-exec-command "show-buffer")
                                  nil `(,buffer t) nil)
      (save-window-excursion
        (switch-to-buffer buffer)
        ;; 余分な改行を削除する。
        (kill-ring-save (point-min) (- (point-max) 1)))
      (kill-buffer buffer))))

(require 'dired)

;; (@> "dired関連のキーバインド設定")
;; m でマークして T で一括変換
(define-key dired-mode-map (kbd "T") 'my:dired-do-convert-coding-system)
;; 自由にリネームを行えるようにする
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)
;; 指定したファイルから、音声ファイルを抽出する
(define-key dired-mode-map (kbd "E") 'my:ffmpeg-extract-audio)

(defun my:dired-up-directory ()
  (interactive)
  (find-alternate-file ".."))

(define-key dired-mode-map (kbd "<backspace>") #'my:dired-up-directory)

;; (@> "dired を使って、一気にファイルの coding system (漢字) を変換する")
;; 現在diredで選択されているファイルに対して、文字コードを変換する。
(defun my:dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write my:dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))


;; dired上から指定した動画から音声のみを抽出する。
(defun my:ffmpeg-extract-audio ()
  (interactive)
  (let* ((ext (file-name-extension (dired-get-filename t)))
         (extract-ext (if (string= "flv" ext) "mp3" "aac"))
         (basename (url-file-extension (dired-get-filename) t)))
    (call-process-shell-command
     (format "/usr/bin/ffmpeg -i \"%s\" -acodec copy \"%s\""
             (dired-get-filename t) (concat basename "." extract-ext)) nil nil t))
  (message (format "extract completed %s" (url-file-extension (dired-get-filename) t)))
  )

;; mode-line
(defface my:face:mode-line-buffer-eol-type
  `((t (:foreground ,(face-attribute 'font-lock-constant-face :foreground))))
  "Face for the EOL type on the mode line"
  :group 'my:customize:face)

;;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
;; 改行文字の文字列表現
(set 'eol-mnemonic-dos "(CRLF)")
(set 'eol-mnemonic-unix "(LF)")
(set 'eol-mnemonic-mac "(CR)")
(set 'eol-mnemonic-undecided "(?)")

;; mozc

(when (and my:use-mozc-el
           (boundp 'my:mozc-helper-locate))
  (defun my:disable-mozc ()
    (interactive)
    (set-input-method nil))

  (defun my:enable-mozc ()
    (interactive)
    (set-input-method 'japanese-mozc))

  (setq-default default-input-method my:input-method)
  (setq default-input-method my:input-method)

  (global-set-key (kbd "<Hangul>") #'my:enable-mozc)
  (global-set-key (kbd "<henkan>") #'my:enable-mozc)
  (global-set-key (kbd "<Hangul_Hanja>") #'my:disable-mozc)
  (global-set-key (kbd "<muhenkan>") #'my:disable-mozc))

(defun my:font-initialize (&optional font-size)
  "Initialize fonts on window-system"
  (interactive "P")

  (let ((font-size (if font-size
                       (read-minibuffer "Font Size:")
                     my:font-size)))
    (when window-system
      (cond
       ((eq window-system 'ns)
        (let* ((size (or font-size my:font-size))
               (asciifont "Cica")
               (jpfont "Cica")
               (h (round (* size 10)))
               (fontspec)
               (jp-fontspec))
          (set-face-attribute 'default nil :family asciifont :height h)
          (setq fontspec (font-spec :family asciifont))
          (setq jp-fontspec (font-spec :family jpfont))
          (set-fontset-font nil 'japanese-jisx0208 jp-fontspec)
          (set-fontset-font nil 'japanese-jisx0212 jp-fontspec)
          (set-fontset-font nil 'japanese-jisx0213-1 jp-fontspec)
          (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
          (set-fontset-font nil '(#x0080 . #x024F) fontspec)
          (set-fontset-font nil '(#x0370 . #x03FF) fontspec)))
       ((eq window-system 'x)
        (let* ((size (or font-size my:font-size))
               (asciifont "Cica")
               (jpfont "Cica")
               (h (round (* size 10)))
               (jp-fontspec (font-spec :family jpfont)))
          (set-face-attribute 'default nil :family asciifont :height h)
          (unless (string= asciifont jpfont)
            (set-fontset-font nil 'unicode jp-fontspec nil 'append))
          (when (featurep 'all-the-icons)
            (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-alltheicon-family)) nil 'append)
            (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-material-family)) nil 'append)
            (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-fileicon-family)) nil 'append)
            (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-faicon-family)) nil 'append)
            (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-octicon-family)) nil 'append)
            (set-fontset-font nil 'unicode (font-spec :family (all-the-icons-wicon-family)) nil 'append))
          (message (format "Setup for %s with %f" asciifont size))))
       (t
        (message "Not have window-system"))))))

(add-hook 'after-init-hook #'my:font-initialize)

(defun my:clipboard-initialize ()
  "Initialize clipboard function if emacs launchs on window-system"
  (interactive)

  (when (not window-system)
    (cond
     ((executable-find "pbcopy")
      (defun copy-from-osx ()
        (shell-command-to-string "pbpaste"))

      (defun paste-to-osx (text)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))

      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx))
     ((executable-find "xsel")
      (defun copy-from-x11 ()
        (shell-command-to-string "xsel -o -b"))

      (defun paste-to-x11 (text)
        (let ((process-connection-type nil))
          (let ((proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
            (process-send-string proc text)
            (process-send-eof proc))))

      (setq interprogram-cut-function 'paste-to-x11)
      (setq interprogram-paste-function 'copy-from-x11)))))

(require 'flyspell)
(require 'ispell)

(defun my:flyspell-skip-ja (beg end info)
  "Tell flyspell to skip a Japanese word.
Call this on `flyspell-incorrect-hook'."
  (string-match my:ispell-regexp-ja (buffer-substring beg end)))

(defun my:flyspell-enable ()
  "The function to enable flyspell in current buffer."
  (interactive)
  (flyspell-mode 1))

;; Use hunspell instead of ispell/aspell
(when (executable-find "hunspell")
  (setq flyspell-default-dictionary "en_US")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)

  ;; for performance
  (setq flyspell-issue-message-flag nil)

  (add-hook 'flyspell-incorrect-hook #'my:flyspell-skip-ja))

;;; configurations for LanguageTool to check english grammer.
;; setup languagetool-commandline
(unless (file-exists-p my:langtool-cli-path)
  (when (eq window-system 'x)

    (make-directory (expand-file-name "~/.emacs.d/share") t)
    (let ((langtool-url (format "https://languagetool.org/download/LanguageTool-%s.zip" my:langtool-version))
          (output "/tmp/LanguageTool.zip"))

      (call-process "curl" nil nil t "-L" "-o" output langtool-url)
      (call-process "unzip" nil nil t "-d" (expand-file-name "~/.emacs.d/share") output)
      (rename-file (format "~/.emacs.d/share/LanguageTool-%s/languagetool-commandline.jar" my:langtool-version)
                   my:langtool-cli-path t))))
