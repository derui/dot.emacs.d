(require 'tuareg)
(require 'company)

;; Load merlin-mode
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (require 'merlin)
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))

;; settings for ocaml
(require 'caml-types)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(defun tuareg-mode-hook-1 ()
  ;; indentation rules
  (setq tuareg-leading-star-in-doc t)

  (make-local-variable 'company-idle-delay)
  (setq company-idle-delay 0.2)
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

  ;; ocamlspot and other keys
  (local-set-key (kbd "C-c t") 'caml-types-show-type)
  (electric-indent-mode 1)
  )

(add-hook 'tuareg-mode-hook 'tuareg-mode-hook-1)
