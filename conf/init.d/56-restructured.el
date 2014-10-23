(require 'rst)
;; settings for markdown text
(autoload 'rst-mode "rst"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))
