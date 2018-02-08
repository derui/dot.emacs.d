;; モードラインに関係するパッケージの設定
(use-package cl-lib)

(use-package uniquify
  :config
  (progn
    (toggle-uniquify-buffer-names 1)
    (setq uniquify-buffer-name-style 'forward)
    (setq uniquify-separator "/")
    (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
    (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
    ))

(defface my:face:mode-line-buffer-eol-type
  `((t (:foreground ,(face-attribute 'font-lock-constant-face :foreground))))
  "Face for the EOL type on the mode line"
  :group 'my:customize:face)

(defvar my:mode-line-buffer-eol-type
  '(:propertize
    (:eval (cl-case (coding-system-eol-type buffer-file-coding-system)
             (0 "LF")
             (1 "CRLF")
             (2 "CR")
             (otherwise "??")))
    face my:face:mode-line-buffer-eol-type)
  "Get the buffer's EOL type")
(put 'my:mode-line-buffer-eol-type 'risky-local-variable t)

(defvar my:mode-line-buffer-coding-type
  '(:propertize
    (:eval (let (coding-system
                 (remove-os-info (cl-function (lambda (string)
                                                (replace-regexp-in-string "-\\(dos\\|unix\\|mac\\)$" "" string)))
                                 ))
             (setq coding-system
                   (replace-regexp-in-string "-with-signature" "(bom)"
                                             (funcall remove-os-info (symbol-name buffer-file-coding-system))))
             (upcase coding-system)))
    face font-lock-constant-face)
  "Get the buffer's coding type without EOL type")
(put 'my:mode-line-buffer-coding-type 'risky-local-variable t)

(defvar my:mode-line-buffer-status
  '(:propertize
    (:eval (concat (if buffer-read-only "r-" "rw")
                   ":"
                   (if (buffer-modified-p) "*" "-")))
    face font-lock-constant-face)
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

(add-hook 'after-change-major-mode-hook 'my:clean-mode-line)

;; Stop inverse color on mode-line if using solarized theme
(when (eq 'solarized my:var:current-theme)
  (set-face-attribute 'mode-line nil :inverse-video nil
                      :box t)
  (set-face-attribute 'mode-line-inactive nil :inverse-video nil
                      :box t))

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                my:mode-line-buffer-coding-type
                "["
                my:mode-line-buffer-eol-type
                "]"
                " "
                my:mode-line-buffer-status
                " "
                mode-line-buffer-identification " "
                "(" (line-number-mode "%l") "," (column-number-mode "%02c") ")"
                ;; Some specific information about the current buffer:
                (vc-mode my:mode-line-vc-info " --") ; VC information
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                ;; Misc information, notably battery state and function name
                " "
                mode-line-misc-info
                ;; And the modes, which I don't really care for anyway
                " " mode-line-modes
                mode-line-end-spaces))
