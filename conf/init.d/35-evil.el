(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)

(setq evil-default-cursor t)

(defun my:evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))
;; 論理行と物理行の移動を入れ替え

(defun evil-toggle-input-method ()
  (interactive)
  (if (not current-input-method)
      (when (not (string= evil-state "emacs"))
        (evil-emacs-state)
        (toggle-input-method))
    (when (string= evil-state "emacs")
      (toggle-input-method)
      (evil-normal-state))))

(global-set-key (kbd "C-\\") 'evil-toggle-input-method)
(require 'mozc)
(when (featurep 'mozc)
  (define-key mozc-mode-map (kbd "C-\\") 'evil-toggle-input-method))

(my:evil-swap-key evil-motion-state-map "j" "gj")
(my:evil-swap-key evil-motion-state-map "k" "gk")

(define-key evil-normal-state-map (kbd ";") 'my:helm)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "TAB") nil)
;; evil-jump-forwardを潰す。
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-insert-state-map (kbd "TAB") 'ac-start)

;; evil-leaderの設定
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "a" #'ace-jump-mode
  "d" #'sunrise
  "D" #'sunrise-cd
  "e" #'find-file
  "b" #'switch-to-buffer
  "g" #'ag
  "#" #'server-edit
  "m" #'(lambda ()
          (interactive)
          (call-interactively 'ag)
          (select-window
           (car (my:get-buffer-window-list-regexp "^\\*ag "))))
  "oo" #'swoop-migemo
  "oO" #'swoop-multi
  "om" #'swoop-migemo
  "f" #'my:helm-project
  )

(evil-mode 1)
