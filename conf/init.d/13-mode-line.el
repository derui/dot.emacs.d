;; モードラインに関係するパッケージの設定
(eval-when-compile
  (require 'cl-lib))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

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

;; 文字エンコーディングの文字列表現
(defun my:coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my:coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my:buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my:coding-system-name-mnemonic code))
         (bom (my:coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

;; `mode-line-mule-info' の文字エンコーディングの文字列表現を差し替える
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my:buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

(defvar my:mode-line-buffer-status
  '(" " (:propertize
         (:eval (concat (if buffer-read-only "r-" "rw")
                        ":"
                        (if (buffer-modified-p) "*" "-")))
         face font-lock-constant-face))
  "Get current buffer status")
(put 'my:mode-line-buffer-status 'risky-local-variable t)

(defvar my:mode-line-vc-info
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'my:mode-line-vc-info 'risky-local-variable t)

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

(defun my:clean-mode-line ()
  (interactive)
  (cl-loop for (mode . mode-str) in mode-line-cleaner-alist
           do
           (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


;; Stop inverse color on mode-line if using solarized theme
(when (eq 'solarized my:var:current-theme)
  (set-face-attribute 'mode-line nil :inverse-video nil
                      :box t)
  (set-face-attribute 'mode-line-inactive nil :inverse-video nil
                      :box t))


;;; disable old configuration
;; (add-hook 'after-change-major-mode-hook 'my:clean-mode-line)
;; (setq mode-line-format
;;       '("%e" mode-line-front-space
;;         ;; Standard info about the current buffer
;;         mode-line-mule-info
;;         " "
;;         my:mode-line-buffer-status
;;         " "
;;         mode-line-buffer-identification " "
;;         "(" (line-number-mode "%l") "," (column-number-mode "%02c") ")"
;;         ;; Some specific information about the current buffer:
;;         (vc-mode my:mode-line-vc-info " --") ; VC information
;;         (flycheck-mode flycheck-mode-line) ; Flycheck status
;;         ;; Misc information, notably battery state and function name
;;         " "
;;         mode-line-misc-info
;;         ;; And the modes, which I don't really care for anyway
;;         " " mode-line-modes
;;         mode-line-end-spaces))

(use-package nyan-mode
  :ensure t
  :custom
  (nyan-animate-nyancat t)
  :hook
  (after-init . nyan-mode))

(use-package doom-modeline
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
