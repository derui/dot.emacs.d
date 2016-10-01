(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)
(require 'avy)
(require 'mozc)

(setq evil-default-cursor t)

(defvar my:evil-colemak nil)
(setq my:evil-colemak t)

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
  (cond
   ((and ime-state (not current-input-method))
    (set-input-method default-input-method)
    (evil-emacs-state))
   (t
    (set-input-method nil)
    (evil-normal-state))))

(defun evil-enable-ime ()
  (interactive)
  (evil-change-input-method t))

(defun evil-disable-ime ()
  (interactive)
  (evil-change-input-method nil))

(global-set-key (kbd "<Hangul>") 'evil-enable-ime)
(global-set-key (kbd "<Hangul_Hanja>") 'evil-disable-ime)
(when (featurep 'mozc)
  (define-key mozc-mode-map (kbd "C-\\") 'evil-toggle-input-method))

(define-key evil-normal-state-map (kbd "s") nil)
(define-key evil-normal-state-map (kbd ";") 'my:helm)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "TAB") nil)
;; evil-jump-forwardを潰す。
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common)

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
  )

;;M-:
(evil-ex-define-cmd "eval" 'eval-expression)
(evil-ex-define-cmd "ev" "eval")

;;C-h k
(evil-ex-define-cmd "describe-key" 'describe-key)
(evil-ex-define-cmd "key" "describe-key")

(let ((prefix "s"))
  (cond
   (my:evil-colemak
    (define-key evil-motion-state-map (kbd "e") #'evil-previous-visual-line)
    (define-key evil-normal-state-map (kbd "e") #'evil-previous-visual-line)
    (define-key evil-visual-state-map (kbd "e") #'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "h") #'evil-backward-char)
    (define-key evil-normal-state-map (kbd "h") #'evil-backward-char)
    (define-key evil-visual-state-map (kbd "h") #'evil-backward-char)
    (define-key evil-motion-state-map (kbd "i") #'evil-forward-char)
    (define-key evil-normal-state-map (kbd "i") #'evil-forward-char)
    (define-key evil-visual-state-map (kbd "i") #'evil-forward-char)
    (define-key evil-motion-state-map (kbd "n") #'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "n") #'evil-next-visual-line)
    (define-key evil-visual-state-map (kbd "n") #'evil-next-visual-line)

    ;; matches
    (define-key evil-motion-state-map (kbd "k") #'evil-search-next)
    (define-key evil-motion-state-map (kbd "K") #'evil-search-previous)
    (define-key evil-normal-state-map (kbd "k") #'evil-search-next)
    (define-key evil-normal-state-map (kbd "K") #'evil-search-previous)

    (define-key evil-motion-state-map (kbd "u") #'evil-insert)
    (define-key evil-motion-state-map (kbd "U") #'evil-insert-line)
    (define-key evil-visual-state-map (kbd "u") #'evil-insert)
    (define-key evil-visual-state-map (kbd "U") #'evil-insert-line)
    (define-key evil-normal-state-map (kbd "u") #'evil-insert)
    (define-key evil-normal-state-map (kbd "U") #'evil-insert-line)

    (define-key evil-normal-state-map (kbd "l") #'undo)

    (define-key evil-motion-state-map (kbd (concat prefix " f")) #'avy-goto-char)
    (define-key evil-motion-state-map (kbd (concat prefix " e")) #'avy-goto-line-above)
    (define-key evil-motion-state-map (kbd (concat prefix " n")) #'avy-goto-line-below)
    (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i)))
   (t
    ;; 論理行と物理行の移動を入れ替え
    (my:evil-swap-key evil-motion-state-map "j" "gj")
    (my:evil-swap-key evil-motion-state-map "k" "gk")
    (define-key evil-motion-state-map (kbd (concat prefix " f")) #'avy-goto-char)
    (define-key evil-motion-state-map (kbd (concat prefix " j")) #'avy-goto-line-below)
    (define-key evil-motion-state-map (kbd (concat prefix " k")) #'avy-goto-line-above))))

(evil-mode 1)
