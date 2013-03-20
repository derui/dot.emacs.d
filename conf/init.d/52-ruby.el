;; (@* "ruby関連の設定")
(el-get 'sync '(ruby-mode
                ruby-electric))

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
             (ruby-electric-mode 1)
             ))

;; ruby-electric-modeのminor-modeにおける優先順を最下位にする。
(let ((rel (assq 'ruby-electric-mode minor-mode-map-alist)))
  (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel))))
