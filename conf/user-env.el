
;; user-defined variables
(defgroup my nil "My custom group" :group 'configuration)
(defcustom my:font-size 10.5 "current theme for me"
  :group 'my
  :type 'symbol)
(defvar my:input-method "japanese-mozc")
(defvar my:save-buffer-hook nil
  "ユーザーが独自に登録可能なsave-buffer-hookへのhook")
(defvar my:last-search-char nil
  "character that is last searched")
(defvar my:last-search-char-direction 'forward
  "direction that is last searched")
(defvar my:tmux-bin-path nil "the executable path of tmux")
(defvar my:dired-default-file-coding-system nil)

(defvar my:dired-file-coding-system 'no-conversion)

(defvar my:ispell-regexp-ja "[一-龠ぁ-🈀ァ-𛀀ー・、。々]+"
  "Regular expression to match a Japanese word.
The expression can be [^\000-\377]+, [^!-~]+, or [一-龠ぁ-🈀ァ-𛀀ー・、。々]+")

(defvar my:langtool-version "4.2")
(defvar my:langtool-cli-path (expand-file-name (locate-user-emacs-file
                                                (format "share/LanguageTool-%s/languagetool-commandline.jar"
                                                        my:langtool-version))))

(defvar my:trailing-whitespace-exclude-modes '(org-mode))

(defvar skk-user-directory (expand-file-name "skk-get-jisyo" user-emacs-directory))

;; user environment specified variable
(defvar my:mozc-el-locate nil)
(defvar my:mozc-helper-locate nil)
(defvar my:rust-src-location nil)
(defvar my:rust-racer-path nil)
(defvar my:virtualenv-path nil)
(defvar my:roswell-path nil)
(defvar my:gtd-base-path nil)
(defvar my:use-mozc-el nil)
(defvar my:migemo-command nil)
(defvar my:migemo-dictionary nil)
(defvar my:use-posframe t)

;; load user-env.el if it exists.
(let ((user-env (locate-user-emacs-file "conf/user-env-specified.el")))
  (when (file-exists-p user-env)
    (load user-env)))

;; exec-pathに必要なパスを追加する。
(add-to-list 'exec-path (expand-file-name "~/.npm/bin"))
(add-to-list 'exec-path (expand-file-name "~/.anyenv/envs/nodenv/shims"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/sbin")
(add-to-list 'exec-path (expand-file-name "bin" my:roswell-path))
