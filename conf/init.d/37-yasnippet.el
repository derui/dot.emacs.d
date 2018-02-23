(eval-when-compile
  (require 'use-package))

;; (@* "yasnippetを利用する設定")
(use-package popup :commands (popup-make-item popup-menu*))
(use-package s :commands s-join)
(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("<C-tab>" . yas-expand))
  :commands (yas-expand yas-global-mode)
  :config

  (defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
    (let ((group-max-len 0)
          (key-max-len 0)
          (fmt "")
          (popup-items))

      (mapc #'(lambda (choice)
                (when (yas--template-p choice)
                  (setq group-max-len (max group-max-len
                                           (+ (length (yas--template-group choice) )
                                              (apply '+ (mapcar 'length (yas--template-group choice))))))
                  (setq key-max-len (max key-max-len (length (yas--template-key choice))))))
            choices)

      (setq fmt (format "%s%%%d.%ds%s%%-%d.%ds  %%s"
                        (if (> group-max-len 0 ) "" " ")
                        group-max-len group-max-len
                        (if (> group-max-len 0 ) " > " "")
                        key-max-len key-max-len))

      (setq popup-items
            (mapcar
             #'(lambda (choice)
                 (popup-make-item
                  (if (yas--template-p choice)
                      (format fmt
                              (if (yas--template-group choice)
                                  (s-join "/" (yas--template-group choice))
                                "")
                              (if (yas--template-key choice)
                                  (yas--template-key choice)
                                "")
                              (if (yas--template-name choice)
                                  (yas--template-name choice)
                                ""))
                    (format " %s" choice))
                  :value choice))
             choices))

      (popup-menu*
       popup-items
       :prompt prompt
       :max-width 80
       :isearch t)))

  (setq yas-prompt-functions '(yas-popup-isearch-prompt))
  (yas-global-mode 1))
