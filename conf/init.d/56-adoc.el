(use-package adoc-mode
  :config
  (progn
    ;; settings for markdown text
    (autoload 'adoc-mode "adoc"
      "Major mode for editing Asciidoc files" t)
    (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
    ))
