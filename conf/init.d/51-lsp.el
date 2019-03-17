;;; -*- lexical-binding: t -*-
(eval-when-compile
  (require 'use-package))

(use-package lsp-mode
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  ;; do not use flymake
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 'incremental) ;; always send incremental document
  (lsp-response-timeout 5)
  (lsp-enable-completion-at-point nil)
  :bind
  (:map lsp-mode-map
        ("C-c r"   . lsp-rename)
        ("<Tab>" . company-indent-or-complete-common))
  :hook
  (lsp-mode . my:lsp-disable-eldoc-when-hover)
  :config
  (require 'lsp-clients)

  (defun my:lsp-disable-eldoc-when-hover ()
    (when (my:minor-mode-active-p 'lsp-mode)
      (setq-local eldoc-message-function (lambda (&rest _) (progn)))))

  ;; LSP UI tools
  (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    ;; lsp-ui-flycheck
    ;; use flycheck on the fly
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions nil)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable nil)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'always) ;; never, on-demand, or always
    :preface
    (defun my:toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-j" . lsp-ui-peek-find-definitions)
          ("C-c i"   . lsp-ui-peek-find-implementation)
          ("C-c m"   . lsp-ui-imenu)
          ("C-c s"   . lsp-ui-sideline-mode)
          ("C-c d"   . my:toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode)
    :config
    (ad-disable-regexp "lsp-ui-doc-.+")
    (ad-activate 'select-window))

  ;; Lsp completion
  (use-package company-lsp
    :after (company)
    :custom
    (company-lsp-cache-candidates 'auto) ;; always using cache
    (company-lsp-async t)
    (company-lsp-enable-recompletion nil)))
