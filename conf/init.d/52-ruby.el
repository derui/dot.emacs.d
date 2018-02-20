(eval-when-compile
  (require 'use-package))

;; (@* "ruby関連の設定")

(use-package ruby-mode)
(use-package ruby-end)

(define-key ruby-mode-map (kbd "C-c x") 'xmp)
(define-key ruby-mode-map (kbd "C-M-i") 'rct-complete-symbol--anything)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))

;; ミニバッファに表示し、かつオーバレイする。
(setq ruby-block-highlight-toggle t)

;; riをemacsから利用するための設定。1.9.1だとfastriは動かない。
(autoload 'ri "ri-ruby.el")
(setq ri-ruby-program "ruby")
(setq ri-ruby-script (concat user-emacs-directory "etc/ri-emacs.rb"))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (ruby-end-mode 1)
             ))
