(use-package tuareg)
(use-package company)

;; Load merlin-mode
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))

    (use-package merlin)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)

    (use-package ocp-indent)))

;; settings for ocaml
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

;; Global tuareg setting
(setq tuareg-let-always-indent t)
(setq tuareg-function-indent 0)
(setq tuareg-match-indent 0)
(setq tuareg-sig-struct-indent 0)
(setq tuareg-begin-indent tuareg-default-indent)
(setq tuareg-match-patterns-aligned t)

(defun tuareg-mode-hook-1 ()
  ;; indentation rules

  (make-local-variable 'company-idle-delay)
  (setq company-idle-delay 0.2)
  ;; ocamlspot and other keys
  (local-set-key (kbd "C-c f") #'ocp-indent-buffer)
  (electric-indent-mode 1)
  )

(add-hook 'tuareg-mode-hook 'tuareg-mode-hook-1)
