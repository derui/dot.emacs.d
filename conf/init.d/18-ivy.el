(eval-when-compile
  (require 'use-package))

(use-package ivy
  :diminish (ivy-mode . "")
  :custom
  (ivy-format-function 'ivy-format-function-arrow)
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 30)
  (ivy-extra-directories nil)
  (ivy-re-builders-alist
   '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
     (t . ivy--regex-plus)))
  (ivy-initial-inputs-alist nil)
  :hook ((after-init . ivy-mode)))

(use-package counsel
  :commands (counsel-rg counsel-ag counsel-grep)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-yank-pop-separator "\n-------\n")
  (counsel-yank-pop-height 30)
  :config
  (defun my:counsel-search-dwim ()
    "Merge version to search document via grep/ag/rg.
Use fast alternative if it exists, fallback grep if no alternatives in system.
"
    (interactive)
    (cond
     ((executable-find "rg") (counsel-rg))
     ((executable-find "ag") (counsel-ag))
     (t (counsel-grep)))))

(use-package swiper
  :bind (("C-s" . swiper))
  :custom
  (swiper-include-line-number-in-search t))

(use-package migemo
  :commands (migemo-init)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  ;; 遅いのを防ぐためにキャッシュする。
  (migemo-use-pattern-alist t)
  (migemo-use-frequent-pattern-alist t)
  (migemo-pattern-alist-length 1024))

(use-package avy-migemo
  :after (migemo)
  :commands (avy-migemo-mode)
  :config
  ;; 初期化する。
  (use-package avy-migemo-e.g.swiper)
  (migemo-init)
  (avy-migemo-mode 1))

(use-package ivy-rich
  :after (ivy)
  :hook ((after-init . ivy-rich-mode))
  :config
  (use-package all-the-icons :commands (all-the-icons-icon-for-mode))

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
               (lambda (cand) (get-buffer cand)))))
