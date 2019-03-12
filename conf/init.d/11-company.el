(eval-when-compile
  (require 'use-package))

(use-package company-quickhelp
  :ensure t
  :custom
  (company-quickhelp-color-foreground "black")
  :hook ((company-mode . company-quickhelp-mode)))

(use-package company-box
  :after (all-the-icons)
  :ensure t
  :hook ((company-mode . company-box-mode))
  :custom
  (company-box-doc-enable nil)
  (company-box-icons-functions '(my:company-box-icons--lsp))
  :config

  (defcustom my:company-box-icons--all-the-icons '() "all-the-icons integrated to company-box"
    :group 'my
    :type 'list)

  (defconst my:company-box-icons--lsp-alist
    '((1 . Text)
      (2 . Method)
      (3 . Function)
      (4 . Constructor)
      (5 . Field)
      (6 . Variable)
      (7 . Class)
      (8 . Interface)
      (9 . Property)
      (10 . Module)
      (11 . Unit)
      (12 . Value)
      (13 . Enum)
      (14 . Keyword)
      (15 . Snippet)
      (16 . Color)
      (17 . File)
      (18 . Reference)
      (19 . Folder)
      (20 . EnumMember)
      (21 . Constant)
      (22 . Struct)
      (23 . Event)
      (24 . Operator)
      (25 . TypeParameter)))

  (setq my:company-box-icons--all-the-icons
        `((Unknown . ,(all-the-icons-faicon "cog"))
          (Text . ,(all-the-icons-octicon "file-text"))
          (Method . ,(all-the-icons-faicon "cube"))
          (Function . ,(all-the-icons-faicon "cube"))
          (Constructor . ,(all-the-icons-faicon "cube"))
          (Field . ,(all-the-icons-faicon "cog"))
          (Variable . ,(all-the-icons-faicon "cog"))
          (Class . ,(all-the-icons-faicon "cogs"))
          (Module . ,(all-the-icons-alltheicon "less"))
          (Property . ,(all-the-icons-faicon "wrench"))
          (Enum . ,(all-the-icons-material "content_copy"))
          (Snippet . ,(all-the-icons-material "content_paste"))
          (Color . ,(all-the-icons-material "palette"))
          (File . ,(all-the-icons-faicon "file"))
          (Folder . ,(all-the-icons-faicon "folder"))
          (Struct . ,(all-the-icons-faicon "cogs"))
          (Event . ,(all-the-icons-faicon "bolt"))
          (TypeParameter . ,(all-the-icons-faicon "cogs"))))

  ;; force replace unknown icon
  (setq company-box-icons-unknown (alist-get 'Unknown my:company-box-icons--all-the-icons))

  (defun my:company-box-icons--lsp (candidate)
    "get the icon that match the type of the candidate on LSP"
    (-when-let* ((lsp-item (get-text-property 0 'lsp-completion-item candidate))
                 (kind (gethash "kind" lsp-item))
                 (type (my:company-box--kind-to-type kind)))
      (alist-get type my:company-box-icons--all-the-icons)))

  (defun my:company-box--kind-to-type (kind)
    "simple function to map kind of LSP to icon type"
    (alist-get kind my:company-box-icons--lsp-alist)))

(use-package company
  :ensure t
  :commands (global-company-mode)
  :diminish (company-mode . "")
  :config
  (global-set-key (kbd "C-M-i") 'company-complete-common-or-cycle)
;;; C-n, C-pで補完候補を選べるように
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; ;のキーで現在選択している候補で保管する
  (define-key company-active-map (kbd ";") 'company-complete-selection)
;;; C-hがデフォルトでドキュメント表示にmapされているので、文字を消せるようにmapを外す
  (define-key company-active-map (kbd "C-h") nil)
;;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
  (define-key company-active-map (kbd "C-M-i") 'company-complete-common-or-cycle)
;;; ドキュメント表示
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)

  (setq company-minimum-prefix-length 1) ;; 1文字入力で補完されるように
;;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)

;;; 色の設定。出来るだけ奇抜にならないように
  (set-face-attribute 'company-tooltip nil
                      :foreground "black"
                      :background "lightgray")
  (set-face-attribute 'company-preview-common nil
                      :foreground "dark gray"
                      :background "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-selection nil
                      :background "steelblue"
                      :foreground "white")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black"
                      :underline t)
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white"
                      :background "steelblue"
                      :underline t)
  (set-face-attribute 'company-tooltip-annotation nil
                      :foreground "red")

  (global-company-mode 1))
