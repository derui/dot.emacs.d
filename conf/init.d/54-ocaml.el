;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'use-package))

(eval-and-compile
  (defun my:opam-share-directory-p ()
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (and opam-share (file-directory-p opam-share))))

  (defun my:opam-load-path ()
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        (expand-file-name "emacs/site-lisp" opam-share)))))

;; Load merlin-mode
(when (my:opam-share-directory-p)
  (add-to-list 'load-path (my:opam-load-path))
  (require 'merlin)
  (require 'merlin-company)
  (require 'merlin-imenu)

  (setq merlin-command 'opam)

  (require 'ocp-indent)
  (autoload 'ocp-indent-buffer "ocp-indent" nil t))

(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :config
  ;; Global tuareg setting
  (setq tuareg-let-always-indent t)
  (setq tuareg-function-indent 0)
  (setq tuareg-match-indent 0)
  (setq tuareg-sig-struct-indent 0)
  (setq tuareg-begin-indent tuareg-default-indent)
  (setq tuareg-match-patterns-aligned t)

  (defun tuareg-mode-hook-1 ()
    ;; indentation rules

    (setq-local company-backends '(company-semantic company-files merlin-company-backend))
    (setq-local company-idle-delay 0.2)
    ;; ocamlspot and other keys
    (define-key tuareg-mode-map (kbd "C-c f") #'ocp-indent-buffer)
    (electric-indent-mode 1)
    (merlin-mode 1)
    (merlin-use-merlin-imenu))

  (add-hook 'tuareg-mode-hook 'tuareg-mode-hook-1))
