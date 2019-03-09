;;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))

;;; window-systemがxの場合に実行される初期化elisp
;; x11を利用している場合、クリップボードの連携を有効にする。
(when (eq window-system 'x)
  (setq select-enable-clipboard t
        select-enable-primary t))

(defvar my:font-size)
(setq my:font-size 10.5)

(use-package all-the-icons
  :ensure t
  :hook ((after-init . my:font-initialize))
  :custom
  (all-the-icons-scale-factor 1.0)
  :config
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
        (let* ((size my:font-size)
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
        (message "Not have window-system")))))
  (my:font-initialize))

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
