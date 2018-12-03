(eval-when-compile
  (require 'use-package))

(use-package plantuml-mode
  :defines (plantuml-output-type plantuml-options)
  :commands (plantuml-mode)
  :config
  (let ((plantuml-jar-file (expand-file-name (locate-user-emacs-file "plantuml.jar"))))
    (unless (file-exists-p plantuml-jar-file)
      (call-process "curl" nil nil t "-L" "-o" plantuml-jar-file
                    "https://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))

    (setq plantuml-output-type "png")
    (setq plantuml-options "-charset UTF-8")
    (setq-default plantuml-jar-path plantuml-jar-file)))
