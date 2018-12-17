(eval-when-compile
  (require 'use-package))

(defun my:evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(use-package evil-iedit-state
  :ensure t
  :commands (evil-iedit-state/iedit-mode))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "i" #'evil-iedit-state/iedit-mode
    "q" #'evil-delete-buffer
    "w" #'save-buffer
    "oc" #'org-capture
    "os" #'org-tree-slide-mode
    "d" #'dired-jump
    "e" #'find-file
    "b" #'ibuffer
    "#" #'server-edit
    "s" #'ag
    "m" #'magit-status
    "f" #'counsel-git
    ;; 'l' is head character of operations for 'lint'
    ;; Recommend to use evil's default keybinding (z =, s ] or s [) when correct warning issued from flyspell.
    "ll" #'langtool-check
    "lL" #'langtool-check-done
    ;; 'c' is head character of 'counsel'
    "ci" #'counsel-imenu
    "cg" #'counsel-git-grep
    "cs" #'counsel-ag
    "cf" #'counsel-git
    "x" #'counsel-M-x
    "z" #'winner-undo))

(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/dec-at-pt evil-numbers/inc-at-pt))

(use-package avy :defer t)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (defun my:evil-define-state-to-all-map (key fun)
    "Define command to key on all state (not include insert state)"

    (define-key evil-normal-state-map key fun)
    (define-key evil-motion-state-map key fun)
    (define-key evil-visual-state-map key fun))

  (defun my:evil-change-input-method (ime-state)
    (let ((when-emacs-state (string= evil-state "emacs")))
      (cond
       ((and ime-state (or (not current-input-method) (string-equal current-input-method my:input-method)))
        (set-input-method my:input-method)
        (when (evil-normal-state-p)
          (evil-insert-state)))
       (t
        (set-input-method nil)))))

  (defun my:evil-enable-ime ()
    (interactive)
    (my:evil-change-input-method t))

  (defun my:evil-disable-ime ()
    (interactive)
    (my:evil-change-input-method nil))

  ;; Disable ime returned to normal state
  (add-hook 'evil-normal-state-entry-hook #'my:evil-disable-ime)

  (define-key evil-normal-state-map (kbd "M-y") #'counsel-yank-pop)
  (define-key evil-normal-state-map (kbd "s") nil)
  (define-key evil-normal-state-map (kbd ";") #'ivy-switch-buffer)
  (define-key evil-normal-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") #'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "TAB") nil)
  ;; evil-jump-forwardを潰す。
  (define-key evil-motion-state-map (kbd "TAB") nil)

  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map (kbd "M-y") #'counsel-yank-pop)
  (define-key evil-insert-state-map (kbd "C-q") #'evil-normal-state)

  (define-key evil-insert-state-map (kbd "<Hangul>") #'my:evil-enable-ime)
  (define-key evil-insert-state-map (kbd "<henkan>") #'my:evil-enable-ime)
  (define-key evil-insert-state-map (kbd "<Hangul_Hanja>") #'my:evil-disable-ime)
  (define-key evil-insert-state-map (kbd "<muhenkan>") #'my:evil-disable-ime)

  ;; for neotree
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") #'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") #'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") #'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") #'neotree-enter)

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

  (setq evil-normal-state-tag   (propertize "N" 'face '((:foreground "black")))
        evil-emacs-state-tag    (propertize "E" 'face '((:foreground "black")))
        evil-insert-state-tag   (propertize "I" 'face '((:foreground "red")))
        evil-motion-state-tag   (propertize "M" 'face '((:foreground "blue")))
        evil-visual-state-tag   (propertize "V" 'face '((:foreground "black")))
        evil-operator-state-tag (propertize "O" 'face '((:foreground "purple"))))

  ;; To suppress error when exit from insert-state
  (setq abbrev-expand-function #'ignore)
  (evil-mode 1))
