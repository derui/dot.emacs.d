###############################################################
# Quick Test - Emacs
###############################################################
[private]
@default: help

@help:
  echo "Usage: just <recipe>"
  echo ""
  just --list

emacs-clean:
  rm -rf ${HOME}/.config/emacs

emacs-test: emacs-clean tangle
  rsync --exclude=.git/ -avz --copy-links --chmod=D2755,F744 . ${HOME}/.config/emacs

tangle-script := '''
  (progn
  (setq debug-on-error t)
  (setq vc-handled-backends nil)
  (require 'ob-tangle)
  (org-babel-tangle-file \"./early-init.org\" \"./early-init.el\" \"emacs-lisp\")
  (org-babel-tangle-file \"./init.org\" \"./init.el\" \"emacs-lisp\")
  (byte-compile-file \"./early-init.el\"))
'''

tangle:
  emacs -Q --batch --eval "{{tangle-script}}"
