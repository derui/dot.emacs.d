(require 'evil-leader)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "d" #'sunrise-cd
  "e" #'find-file
  "b" #'switch-to-buffer)
