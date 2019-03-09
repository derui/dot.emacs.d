(eval-when-compile
  (require 'use-package))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :custom
  (ivy-format-function 'ivy-format-function-arrow)
  (counsel-yank-pop-separator "\n-------\n")
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 30)
  (ivy-extra-directories nil)
  :config
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
          (t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq counsel-yank-pop-height 30))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  :config
  (defvar swiper-include-line-number-in-search t))

(use-package migemo
  :commands (migemo-init)
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
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

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :config
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  (plist-put ivy-rich--display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon :width 2)
                (ivy-rich-candidate (:width 30))
                (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 15 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))))

  (ivy-rich-mode 1))
