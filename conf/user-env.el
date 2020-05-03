
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
(defvar my:dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")

(defvar my:dired-file-coding-system 'no-conversion)

(defvar my:mode-line-buffer-status
  '(" " (:propertize
         (:eval (concat (if buffer-read-only "r-" "rw")
                        ":"
                        (if (buffer-modified-p) "*" "-")))
         face font-lock-constant-face))
  "Get current buffer status")

;;; Cleaner for long mode name
(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . " Ys")
    (paredit-mode . " Pe")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (helm-mode . "")
    (undo-tree-mode . " Ut")
    (elisp-slime-nav-mode . " EN")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    (git-gutter-mode . " GG")

    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))


(defvar my:mode-line-vc-info
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")

(defvar my:ispell-regexp-ja "[一-龠ぁ-🈀ァ-𛀀ー・、。々]+"
  "Regular expression to match a Japanese word.
The expression can be [^\000-\377]+, [^!-~]+, or [一-龠ぁ-🈀ァ-𛀀ー・、。々]+")

(defvar my:langtool-version "4.2")
(defvar my:langtool-cli-path (expand-file-name (locate-user-emacs-file
                                                (format "share/LanguageTool-%s/languagetool-commandline.jar"
                                                        my:langtool-version))))


(defvar my:org-clocked-time-mode-line "")
(put 'my:org-clocked-time-mode-line 'risky-local-variable t)

;; user environment specified variable
(defvar my:mozc-el-locate nil)
(defvar my:mozc-helper-locate nil)
(defvar my:rust-src-location nil)
(defvar my:rust-racer-path nil)
(defvar my:virtualenv-path nil)
(defvar my:roswell-path nil)
(defvar my:gtd-base-path nil)
(defvar my:use-company-lsp t)
(defvar my:use-mozc-el nil)
(defvar my:migemo-command nil)
(defvar my:migemo-dictionary nil)

;; load user-env.el if it exists.
(let ((user-env (locate-user-emacs-file "conf/user-env-specified.el")))
  (when (file-exists-p user-env)
    (load user-env)))

;; exec-pathに必要なパスを追加する。
(add-to-list 'exec-path "/home/derui/.npm/bin")
(add-to-list 'exec-path "/home/derui/.nodebrew/current/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/sbin")
(add-to-list 'exec-path (expand-file-name "bin" my:roswell-path))
