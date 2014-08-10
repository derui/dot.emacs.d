;; -*- coding: utf-8 -*-
;;; window-systemがxの場合に実行される初期化elisp
;; x11を利用している場合、クリップボードの連携を有効にする。
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mew")

(load-theme 'solarized-dark t)

;; emacs23以降のフォント設定を行う。
(defvar emacs23-font-name "ricty")
(setq emacs23-font-name "ricty")
(let ((paw16a "-paw16a-fixed-medium-r-normal--16-*-*-*-c-*-*")
      (paw16k "-paw16k-fixed-medium-r-normal--16-*-*-*-c-*-*")
      ipa pawfont-name)
  (cond
   ((eq window-system 'ns)
    (let* ((size 12)
           (asciifont "Menlo")
           (jpfont "Hiragino Maru Gothic ProN")
           (h (* size 10))
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
   ((and (eq window-system 'x) (string= emacs23-font-name "ricty"))
    (let* ((fontset-name "myfonts")
           (size 11)
           (h (round (* size 10)))
           (asciifont "Ricty")
           (jpfont "Ricty")
           (font (format "%s:size=%d" asciifont size))
           (fontspec (font-spec :family asciifont :height h :spacing 'm))
           (jp-fontspec (font-spec :family jpfont :height h :spacing 'm))
           (fsn (create-fontset-from-ascii-font font))
           )
      (set-face-attribute 'default nil :family "Ricty" :height h)
      (set-fontset-font fsn 'unicode jp-fontspec)
      ;; (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec nil 'append)
      ;; (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec nil 'append)
      ;; (set-fontset-font fsn 'japanese-jisx0208 jp-fontspec nil 'append)
      ;; (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec nil 'append)
      (add-to-list 'default-frame-alist `(font . ,fsn))
      )
    )
   ((and (eq window-system 'x) (not (string= emacs23-font-name "ricty")))
    (setq pawfont-name (create-fontset-from-ascii-font
                        paw16a))

    (set-fontset-font pawfont-name
                      'latin-jisx0201
                      nil)
    (set-fontset-font pawfont-name
                      'latin-iso8859-1
                      nil)

    (set-fontset-font pawfont-name
                      'japanese-jisx0208
                      paw16k)
    ;; 文字を追加する。
    (set-fontset-font pawfont-name
                      'katakana-jisx0201
                      paw16a)
    ;; emacs23 では pawfontのチルダと円マークが表示できないため、仕方無
    ;; しに、字形の似ている文字を代する。 by rubikitch
    (setq standard-display-table (or standard-display-table (make-display-table)))
    (aset standard-display-table ?\\ [?\x18])
    (aset standard-display-table ?~ [?\x7f])

    (set-frame-font pawfont-name))))

(when (featurep 'pomodoro)

  (require 'notifications) ;; Linuxで DBUSがある環境のみ
  (defun* my:pomodoro-notification (&key (title "Pomodoro")
                                         body
                                         (urgency 'critical))
    (notifications-notify :title title :body body :urgency urgency))

  ;; 作業終了後の hook
  (add-hook 'pomodoro:finish-work-hook
            (lambda ()
              (my:pomodoro-notification :body "Work is Finish")))

  ;; 休憩終了後の hook
  (add-hook 'pomodoro:finish-rest-hook
            (lambda ()
              (my:pomodoro-notification :body "Break time is finished")))

  ;; 長期休憩後の hook
  (add-hook 'pomodoro:long-rest-hook
            (lambda ()
              (my:pomodoro-notification :body "Long Break time now")))
  )


(when (not window-system)
  (cond
   ((executable-find "pbcopy")
    (defun copy-from-osx ()
      (shell-command-to-string "pbpaste"))

    (defun paste-to-osx (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))
   ((executable-find "xsel")
    (defun copy-from-x11 ()
      (shell-command-to-string "xsel -o -b"))

    (defun paste-to-x11 (text &optional push)
      (let ((process-connection-type nil))
        (let ((proc (start-process "xsel" "*Messages*" "xsel" "-i" "-b")))
          (process-send-string proc text)
          (process-send-eof proc))))

    (setq interprogram-cut-function 'paste-to-x11)
    (setq interprogram-paste-function 'copy-from-x11))
   ))
