(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)
(require 'avy)
(require 'mozc)

(setq evil-default-cursor t)

(defun my:evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(defun my:evil-define-state-to-all-map (key fun)
  "Define command to key on all state (not include insert state)"

  (define-key evil-normal-state-map key fun)
  (define-key evil-motion-state-map key fun)
  (define-key evil-visual-state-map key fun))

(defun evil-change-input-method (ime-state)
  (let ((when-emacs-state (string= evil-state "emacs")))
    (cond
     ((and ime-state (not current-input-method))
      (set-input-method my:input-method)
      (when (evil-normal-state-p)
        (evil-insert-state)))
     ((not ime-state)
      (set-input-method nil)))))

(defun evil-enable-ime ()
  (interactive)
  (evil-change-input-method t))

(defun evil-disable-ime ()
  (interactive)
  (evil-change-input-method nil))

(global-set-key (kbd "<Hangul>") 'evil-enable-ime)
(global-set-key (kbd "<henkan>") 'evil-enable-ime)
(global-set-key (kbd "<Hangul_Hanja>") 'evil-disable-ime)
(global-set-key (kbd "<muhenkan>") 'evil-disable-ime)

(define-key evil-normal-state-map (kbd "s") nil)
(define-key evil-normal-state-map (kbd ";") 'my:helm)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "TAB") nil)
;; evil-jump-forwardを潰す。
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common)
(define-key evil-insert-state-map (kbd "jj") 'evil-force-normal-state)

;; evil-leaderの設定
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "q" #'evil-delete-buffer
  "w" #'save-buffer
  "d" #'sunrise
  "D" #'sunrise-cd
  "e" #'find-file
  "b" #'switch-to-buffer
  "#" #'server-edit
  "sg" #'ag
  "sG" #'helm-do-ag
  "f" #'helm-ls-git-ls
  "y" #'helm-yas-complete
  "x" #'helm-M-x
  )

;;M-:
(evil-ex-define-cmd "eval" 'eval-expression)
(evil-ex-define-cmd "ev" "eval")

;;C-h k
(evil-ex-define-cmd "describe-key" 'describe-key)
(evil-ex-define-cmd "key" "describe-key")

    ;; 論理行と物理行の移動を入れ替え
    (my:evil-swap-key evil-motion-state-map "j" "gj")
    (my:evil-swap-key evil-motion-state-map "k" "gk")
    (define-key evil-motion-state-map (kbd "s f") #'avy-goto-char)
    (define-key evil-motion-state-map (kbd (concat "s" " j")) #'avy-goto-line-below)
    (define-key evil-motion-state-map (kbd (concat "s" " k")) #'avy-goto-line-above)

(evil-mode 1)
