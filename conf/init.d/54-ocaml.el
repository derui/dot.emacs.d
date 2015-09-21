(require 'auto-complete)
(require 'tuareg)
(require 'caml)
(require 'ocamlspot)
;; Load merlin-mode
(require 'merlin)

;; settings for ocaml
(require 'caml-types)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(defun tuareg-mode-hook-1 ()
  ;; indentation rules
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
  (setq tuareg-match-indent 0)
  (setq tuareg-begin-indent tuareg-default-indent)
  (setq tuareg-use-smie nil)

  (setq omake-error-highlight-background "#444400")
  (define-key tuareg-mode-map (kbd "M-O") 'omake-run)
  (define-key tuareg-mode-map (kbd "M-R") 'omake-rerun)
  (define-key tuareg-mode-map (kbd "M-P") 'omake-previous-error)
  (define-key tuareg-mode-map (kbd "M-N") 'omake-next-error)
  (define-key tuareg-mode-map (kbd "M-O") 'omake-run)

  ;; ocamlspot and other keys
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
  (electric-indent-mode 1)

  (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

  ;; Start merlin on ocaml files
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  ;; Enable auto-complete
  (setq merlin-use-auto-complete-mode 'easy)
  ;; Use opam switch to lookup ocamlmerlin binary
  (setq merlin-command 'opam)
  )
(add-hook 'tuareg-mode-hook 'tuareg-mode-hook-1)
