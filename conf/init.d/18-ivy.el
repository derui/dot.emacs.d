(require 'use-package)

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-height 30)
    (setq ivy-extra-directories nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-plus)))))

(use-package counsel
  :config
  (progn
    (global-set-key (kbd "M-x") #'counsel-M-x)
    (global-set-key (kbd "C-x C-f") #'counsel-find-file)
    (defvar counsel-yank-pop-height 30)))

(use-package swiper
  :config
  (progn
    (global-set-key (kbd "C-s") #'swiper)
    (defvar swiper-include-line-number-in-search t)))

(use-package migemo)
(use-package avy-migemo :defer t
  :config
  (progn
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    ;; 遅いのを防ぐためにキャッシュする。
    (setq migemo-use-pattern-alist t)
    (setq migemo-use-frequent-pattern-alist t)
    (setq migemo-pattern-alist-length 1024)

    ;; 初期化する。
    (migemo-init)
    (avy-migemo-mode 1)))

(use-package avy-migemo-e.g.swiper :defer t)
