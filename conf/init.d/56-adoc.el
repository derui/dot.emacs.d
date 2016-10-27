(require 'adoc-mode)
;; settings for markdown text
(autoload 'adoc-mode "adoc"
  "Major mode for editing Asciidoc files" t)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
