(eval-when-compile
  (require 'use-package))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq counsel-yank-pop-height 30))

(use-package swiper
  :bind (("C-s" . swiper))
  :config
  (defvar swiper-include-line-number-in-search t))

(use-package migemo
  :commands (migemo-init)
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  ;; 遅いのを防ぐためにキャッシュする。
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024))

(use-package avy-migemo
  :ensure t
  :config
  ;; 初期化する。
  (require 'avy-migemo-e.g.swiper)
  (migemo-init)
  (avy-migemo-mode 1))
