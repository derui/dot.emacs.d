(eval-when-compile
  (require 'use-package))

(use-package plantuml-mode
  :commands (plantuml-mode)
  :config
  (let ((plantuml-jar-file (locate-user-emacs-file "plantuml.jar")))
    (unless (file-exists-p plantuml-jar-file)
      (call-process "curl" nil nil t "-L" "-o" plantuml-jar-file
                    "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))

    (setq-default plantuml-jar-path plantuml-jar-file)))
