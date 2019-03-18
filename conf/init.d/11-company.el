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
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-doc-enable nil))

(use-package company
  :ensure t
  :commands (global-company-mode)
  :diminish (company-mode . "")
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1) ;; 1文字入力で補完されるように
  ;; 候補の一番上でselect-previousしたら一番下に、一番下でselect-nextしたら一番上に行くように
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  :config
  (global-set-key (kbd "C-M-i") 'company-complete-common-or-cycle)
  ;; C-n, C-pで補完候補を選べるように
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  ;; C-hがデフォルトでドキュメント表示にmapされているので、文字を消せるようにmapを外す
  (define-key company-active-map (kbd "C-h") nil)
  ;; 1つしか候補がなかったらtabで補完、複数候補があればtabで次の候補へ行くように
  (define-key company-active-map (kbd "C-M-i") 'company-complete-common-or-cycle)
  ;; ドキュメント表示
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)


  ;; 色の設定。出来るだけ奇抜にならないように
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
