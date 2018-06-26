(unless (featurep 'use-package)
  (package-install 'use-package))

(package-install 'ag)
(package-install 'wgrep)
(package-install 'wgrep-ag)

;; Utility package
(package-install 'f)
(package-install 's)
(package-install 'auto-async-byte-compile)
(package-install 'ert-expectations)
(package-install 'expand-region)
(package-install 'diminish)

;; OCaml
(package-install 'tuareg)

;; Emacs lisp/lisp
(package-install 'aggressive-indent)

;; Typescript
(package-install 'company)
(package-install 'tide)
(package-install 'typescript-mode)

;; JavaScript
(package-install 'js2-mode)
(package-install 'js2-refactor)

;; ivy/counsel
(package-install 'counsel)
(package-install 'swiper)
(package-install 'ivy-hydra)

;; recentf-ext
(package-install 'recentf-ext)

;; Undo
(package-install 'undo-tree)

;; key utilities
(package-install 'key-chord)

;; buffer
(package-install 'popup)

;; filer
                                        ;(package-install 'sunrise-commander)
(package-install 'neotree)

;; evil
(package-install 'evil)
(package-install 'evil-leader)
(package-install 'evil-numbers)
(package-install 'evil-cleverparens)
(package-install 'evil-iedit-state)

;; highlight
(package-install 'highlight)

;; Bookmark
(package-install 'bm)

;; VCS
(package-install 'magit)
(package-install 'git-gutter)

;; snippet
(package-install 'yasnippet)

;; list editing
(package-install 'zlc)

;; style modes
(package-install 'stylus-mode)

;; go lang
(package-install 'go-mode)
(package-install 'go-eldoc)
(package-install 'company-go)

;; markdown
(package-install 'markdown-mode)

;; lua
(package-install 'lua-mode)

;; scala
(package-install 'ensime)

;; haskell
(package-install 'haskell-mode)

;; C
(package-install 'google-c-style)

;; ruby
(package-install 'ruby-end)

;; python
(package-install 'elpy)

;; Web Mode
(package-install 'web-mode)
(package-install 'yaml-mode)
(package-install 'rainbow-mode)

;; Themes
(package-install 'color-theme-solarized)

;; Shell
(package-install 'exec-path-from-shell)

;; quickrun
(package-install 'quickrun)

(package-install 'flycheck)

;; org-mode
(package-install 'org-pomodoro)
(package-install 'org-tree-slide)

;; avy
(package-install 'avy)

;; migemo
(package-install 'migemo)
(package-install 'avy-migemo)

;; log4e
(package-install 'log4e)

;; rust
(package-install 'rust-mode)

;; clojure
(package-install 'clojure-mode)
(package-install 'cider)
(package-install 'smartparens)
(package-install 'clj-refactor)

;; asciidoc
(package-install 'adoc-mode)

;; groovy
(package-install 'groovy-mode)

(package-install 'symbol-overlay)

(package-install 'terraform-mode)

;; common lisp
(package-install 'slime-company)

(package-install 'shackle)

;; for Mozc
(package-install 'mozc-popup)
