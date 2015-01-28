(require 'auto-complete)
(require 'tuareg)
(require 'caml)

;; settings for ocaml
;; typerex2の設定、インストールされていない場合は、tuareg + ocamlspotの設定
(if (executable-find "ocp-edit-mode")
    (progn
      (defun tuareg-mode-hook-1 ()
        (make-local-variable 'ac-auto-start)
        (setq ac-auto-start 2)
        (setq tuareg-leading-star-in-doc t)
        (setq tuareg-in-indent 0)
        (setq tuareg-let-always-indent t)
        (setq tuareg-let-indent tuareg-default-indent)
        (setq tuareg-with-indent 0)
        (setq tuareg-function-indent 0)
        (setq tuareg-fun-indent 0)
        (setq tuareg-parser-indent 0)
        (setq tuareg-match-indent 0)
        (setq tuareg-begin-indent tuareg-default-indent)
        (setq tuareg-parse-indent tuareg-default-indent); .mll
        (setq tuareg-rule-indent  tuareg-default-indent)
        (setq tuareg-use-smie nil)
        )
      (add-hook 'tuareg-mode-hook 'tuareg-mode-hook-1)
      (with-temp-buffer
        (insert (shell-command-to-string "ocp-edit-mode emacs -load-global-config"))
        (eval-buffer)))

  ;; typerex did not be installed.
  (progn
    (require 'caml-types)
    (setq auto-mode-alist
          (append '(("\\.ml[ily]?$" . tuareg-mode)
                    ("\\.topml$" . tuareg-mode))
                  auto-mode-alist))
    (defun tuareg-mode-hook-1 ()
      ;; indentation rules
      (setq tuareg-lazy-= t)
      (setq tuareg-lazy-paren t)
      (setq tuareg-electric-indent t)
      (setq tuareg-leading-star-in-doc t)

      (make-local-variable 'ac-auto-start)
      (setq ac-auto-start 2)
      (setq tuareg-leading-star-in-doc t)
      (setq tuareg-in-indent 0)
      (setq tuareg-let-always-indent t)
      (setq tuareg-let-indent tuareg-default-indent)
      (setq tuareg-with-indent 0)
      (setq tuareg-function-indent 0)
      (setq tuareg-fun-indent 0)
      (setq tuareg-parser-indent 0)
      (setq tuareg-match-indent 0)
      (setq tuareg-begin-indent tuareg-default-indent)
      (setq tuareg-parse-indent tuareg-default-indent); .mll
      (setq tuareg-rule-indent  tuareg-default-indent)
      (setq tuareg-use-smie nil)

      (setq omake-error-highlight-background "#444400")
      (define-key tuareg-mode-map (kbd "M-O") 'omake-run)
      (define-key tuareg-mode-map (kbd "M-R") 'omake-rerun)
      (define-key tuareg-mode-map (kbd "M-P") 'omake-previous-error)
      (define-key tuareg-mode-map (kbd "M-N") 'omake-next-error)
      (define-key tuareg-mode-map (kbd "M-O") 'omake-run)

      ;; ocamlspot and other keys
      (require 'ocamlspot)
      (local-set-key (kbd "C-c ;") 'ocamlspot-query)
      (local-set-key (kbd "C-c :") 'ocamlspot-query-interface)
      (local-set-key (kbd "C-c '") 'ocamlspot-query-uses)
      (local-set-key (kbd "C-c C-t") 'ocamlspot-type)
      (local-set-key (kbd "C-c C-i") 'ocamlspot-xtype)
      (local-set-key (kbd "C-c C-y") 'ocamlspot-type-and-copy)
      (local-set-key (kbd "C-c x") 'ocamlspot-expand)
      (local-set-key (kbd "C-c C-u") 'ocamlspot-use)
      (local-set-key (kbd "C-c t") 'caml-types-show-type)
      (local-set-key (kbd "C-c p") 'ocamlspot-pop-jump-stack)
      )
    (add-hook 'tuareg-mode-hook 'tuareg-mode-hook-1)
    ))
