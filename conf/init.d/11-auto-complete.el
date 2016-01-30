(require 'popup)
(require 'fuzzy)
(require 'auto-complete)
;; 補完候補が一つだけだったら次の行に移るようにする。

(defun my:ac-next-or-next-line (arg)
  (interactive "p")
  (if (/= (length ac-candidates) 1)
      (ac-next)
    (ac-abort)
    (next-line arg)))
(defun my:ac-previous-or-previous-line (arg)
  (interactive "p")
  (if (/= (length ac-candidates) 1)
      (ac-previous)
    (ac-abort)
    (previous-line arg)))

;; auto-completeの際に、日本語が含まれないようにする。
(defadvice ac-candidate-words-in-buffer (after remove-word-contain-japanese activate)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value
          (remove-if contain-japanese ad-return-value))))

;; c-mなどを、auto-completeで候補選択している場合にのみauto-completeの機能として利用できるようにする。
;; DO NOT have `ac-' prefix
(defun my:ac-fallback ()
  (interactive)
  (ac-fallback-command 'my:ac-fallback))

;; MUST have `ac-' prefix
(defun ac-my:complete ()
  (interactive)
  (if my:ac-explicit-complete
      (ac-complete)
    (ac-abort)
    (ac-fallback-command)))

(defvar my:ac-explicit-complete nil)
(defvar my:ac-explicit-complete-commands
  '(ac-next ac-previous ac-isearch))

(defun my:ac-set-explicit-complete ()
  (setq my:ac-explicit-complete
        (memq this-command my:ac-explicit-complete-commands)))

;; auto-complete開始時に実行される処理。
(defun my:auto-complete-mode-hook-0 ()
  (define-key ac-complete-mode-map (kbd ";") 'ac-complete)
  ;; 候補が1つしかない場合には、もともとのC-n/C-pの機能となるようにする
  (define-key ac-completing-map (kbd "C-n") 'my:ac-next-or-next-line)
  (define-key ac-completing-map (kbd "C-p") 'my:ac-previous-or-previous-line)
  (setq ac-use-menu-map t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-menu-map (kbd "C-s") 'ac-isearch)

  (define-key ac-completing-map (kbd "C-i") 'my:ac-fallback)
  (define-key ac-completing-map (kbd "C-s") 'my:ac-fallback)

  (define-key ac-completing-map (kbd "C-r") 'ac-my:complete)
  (add-hook 'post-command-hook 'my:ac-set-explicit-complete))

(add-hook 'auto-complete-mode-hook 'my:auto-complete-mode-hook-0)

;; 大文字小文字の区別は行わない。
(setq ac-ignore-case t)
;; 推測補完を行う
(setq ac-dwim t)
;; 初期で利用するauto-completeのsource
(setq ac-sources '(ac-source-words-in-same-mode-buffers))
;; コメントや文字列の中でも補完が動作するように
(setq ac-disable-faces nil)
