;;; IME(SKK, sekka, mozc...)についての設定を記述する。

;; (@* "SKKについての設定")

(defvar skk-user-directory nil)
(defvar skk-isearch-start-mode nil)
;; SKK固有の設定については、~/.emacs.d/etc/skkに設定を記述する
(setq skk-user-directory (concat user-emacs-directory "/etc/skk"))
;; migemoを利用するため、isearchの際には、ddskkには働かないでいてもらう
(setq skk-isearch-start-mode 'latin)

(require 'skk-setup)

(global-set-key [henkan] 'skk-mode)
(setq default-input-method "japanese-skk")
(setq skk-preload t)

(let ((mozc-hooks '(org-mode-hook tuareg-mode-hook js2-mode-hook
                                  emacs-lisp-mode)))
  (when (and (boundp 'my:mozc-el-locate)
             (boundp 'my:mozc-helper-locate))
    (load my:mozc-el-locate)
    (setq default-input-method "japanese-mozc")
    (setq mozc-helper-program-name my:mozc-helper-locate)
    (require 'ac-mozc)

    (defun my:ac-mozc-setup ()
      (add-to-list 'ac-sources 'ac-source-mozc)
      (make-local-variable 'ac-auto-show-menu)
      (setq ac-auto-show-menu 0.2))

    (cl-mapcar #'(lambda (hook)
                   (add-hook hook 'my:ac-mozc-setup))
               mozc-hooks)))
