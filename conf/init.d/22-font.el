;; -*- coding: utf-8 -*-
;;; window-systemがxの場合に実行される初期化elisp
;; x11を利用している場合、クリップボードの連携を有効にする。
(when (eq window-system 'x)
  (setq x-select-enable-clipboard t))

;; emacs23以降のフォント設定を行う。
(defvar my:font-size 13.5)
(setq my:font-size 13.5)

(defun my:font-initialize ()
  "Initialize fonts on window-system"
  (interactive)

  (when window-system
    (cond
     ((eq window-system 'ns)
      (let* ((size my:font-size)
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
     ((eq window-system 'x)
      (let* ((fontset-name "myfonts")
             (size my:font-size)
             (h (round (* size 10)))
             (asciifont "Ricty")
             (jpfont "Ricty")
             (font (format "%s:size=%d" asciifont size))
             (fontspec (font-spec :family asciifont :height h :spacing 'm))
             (jp-fontspec (font-spec :family jpfont :height h :spacing 'm))
             (fsn (create-fontset-from-ascii-font font)))
        (set-face-attribute 'default nil :family "Ricty" :height h)
        (set-fontset-font fsn 'unicode jp-fontspec)
        ;; (set-frame-parameter nil 'font fsn)
        ;; (add-to-list 'initial-frame-alist `(font . ,fsn))
        ;; (add-to-list 'default-frame-alist `(font . ,fsn))
        (message "Setup for Ricty")))
     (t
      (message "Not have window-system")))))

(my:font-initialize)

(when (featurep 'pomodoro)

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
              (my:pomodoro-notification :body "Long Break time now"))))

(defun my:clipboard-initialize ()
  "Initialize clipboard function if emacs launchs on window-system"
  (interactive)
  
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
      (setq interprogram-paste-function 'copy-from-x11)))))