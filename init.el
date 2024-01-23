;; -*- coding: utf-8 -*-
(progn (require 'org) (org-babel-tangle-file (expand-file-name "init.org" user-emacs-directory) (expand-file-name "init.el" user-emacs-directory)) (load (expand-file-name "init.el" user-emacs-directory)) (message "Once kill emacs for apply new init.el written from init.org") (setq kill-emacs-hook nil) (kill-emacs nil t))
