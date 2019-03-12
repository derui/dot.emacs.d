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

  ;; Warn: re-define company-box's internal function to get more performance
  (defun company-box--set-frame-position (frame)
    (-let* (((left top _right _bottom) (company-box--edges))
            (char-height (frame-char-height frame))
            (char-width (frame-char-width frame))
            (height (* (min company-candidates-length company-tooltip-limit) char-height))
            (frame-resize-pixelwise t)
            (mode-line-y (company-box--point-bottom))
            ((p-x . p-y) (company-box--prefix-pos))
            (p-y-abs (+ top p-y))
            (y (or (and (> p-y-abs (/ mode-line-y 2))
                        (<= (- mode-line-y p-y) (+ char-height height))
                        (> (- p-y-abs height) 0)
                        (- p-y height))
                   (+ p-y char-height)))
            (height (or (and (> y p-y)
                             (> height (- mode-line-y y))
                             (- mode-line-y y))
                        height))
            (height (- height (mod height char-height)))
            (x (if company-box--with-icons-p
                   (- p-x (* char-width (if (= company-box--space 2) 2 3)))
                 (- p-x (if (= company-box--space 0) 0 char-width)))))
      ;; Debug
      ;; (message "X+LEFT: %s P-X: %s X: %s LEFT: %s space: %s with-icon: %s LESS: %s"
      ;;          (+ x left) p-x x left company-box--space company-box--with-icons-p (+ (* char-width 3) (/ char-width 2)))
      (setq company-box--x (+ x left)
            company-box--start (or company-box--start (window-start))
            company-box--height height)
      (set-frame-size frame (company-box--update-width t (/ height char-height))
                      height t)
      ;; FIX: avoid to call frequentry set-frame-position that makes display very slowly
      (let* ((pos (frame-position frame))
             (current-x (car pos))
             (current-y (cdr pos))
             (next-x (max (+ x left)))
             (next-y (+ y top)))
        (unless (and (= next-x current-x) (= next-y current-y))
          (set-frame-position frame next-x next-y)))
      (set-frame-parameter frame 'company-box-window-origin (selected-window))
      (set-frame-parameter frame 'company-box-buffer-origin (current-buffer))
      (with-selected-frame frame (set-fringe-style 0))))

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
