(eval-when-compile
  (require 'use-package)
  (require 'cl-lib))

;;; (@* "common lisp関係の設定")
(use-package company
  :commands (company-mode-on))

(use-package evil-cleverparens
  :commands (evil-cleverparens-mode))

(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(use-package lisp-mode
  :ensure nil
  :config
  (defun my:lisp-hooks ()
    (setq-local company-idle-delay 0.2)
    (setq-local show-paren-style 'expression)

    (aggressive-indent-mode)
    (evil-cleverparens-mode)
    (company-mode-on)
    (set-newline-and-indent))

  (add-hook 'lisp-mode-hook 'my:lisp-hooks))

(let ((helper (expand-file-name "helper.el" my:roswell-path)))
  (when (and (file-exists-p helper)
             my:roswell-path)
    (defvar roswell-slime-contribs '(slime slime-fancy slime-company))
    (load helper)
    (add-to-list 'exec-path (expand-file-name "bin" my:roswell-path))

    (defun slime-qlot-exec (directory)
      "start slime with qlot"
      (slime-start :program "qlot"
                   :program-args '("exec" "ros" "-S" "." "run")
                   :directory directory
                   :name 'qlot
                   :env (list (concat "PATH="
                                      (mapconcat 'identity exec-path ":"))
                              (concat "QUICKLISP_HOME="
                                      (file-name-as-directory directory) "quicklisp/"))))

    (defun slime-qlot (directory)
      "start slime with qlot"
      (interactive (list (read-directory-name "Project directory: ")))
      (slime-qlot-exec directory))

    (defun slime-qlot-restart (directory)
      (interactive (list (read-directory-name "Project directory: ")))
      (process-kill-buffer-query-function)
      (ignore-errors
        (let* ((buffer (get-buffer  inferior-lisp-buffer))
               (process (get-buffer-process buffer)))
          (when (and buffer process)
            (set-process-query-on-exit-flag process nil)
            (kill-buffer buffer))))
      (slime-qlot-exec directory))))

(when (featurep 'slime)
  (require 'hyperspec)

  ;; HyperSpecをewwで見る設定
  (setq common-lisp-hyperspec-root "~/.emacs.d/share/HyperSpec/")

  (unless (file-exists-p (expand-file-name "~/.emacs.d/share/HyperSpec"))
    (when (eq window-system 'x)

      (make-directory (expand-file-name "~/.emacs.d/share") t)
      (let ((hyperspec-url "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz")
            (output "/tmp/HyperSpec.tar.gz"))

        (call-process "curl" nil nil t "-L" "-o" output hyperspec-url)
        (shell-command (format "tar zxvf %s -C %s" output "~/.emacs.d/share")))))

  (defun common-lisp-hyperspec (symbol-name)
    (interactive (list (common-lisp-hyperspec-read-symbol-name)))
    (let ((name (common-lisp-hyperspec--strip-cl-package
                 (downcase symbol-name))))
      (cl-maplist (lambda (entry)
                    (eww-open-file (concat common-lisp-hyperspec-root "Body/"
                                           (car entry)))
                    (when (cdr entry)
                      (sleep-for 1.5)))
                  (or (common-lisp-hyperspec--find name)
                      (error "The symbol `%s' is not defined in Common Lisp"
                             symbol-name)))))

  (defadvice common-lisp-hyperspec (around common-lisp-hyperspec-around activate)
    (let ((buf (current-buffer)))
      ad-do-it
      (switch-to-buffer buf)
      (display-buffer "*eww*"))))
