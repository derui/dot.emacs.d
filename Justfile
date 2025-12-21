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

update-lock: emacs-test
  #!/usr/bin/env bash
  set -euo pipefail
  
  # Remove existing lock file if it exists
  rm -f ${HOME}/.config/emacs/elpaca.lock
  
  echo "======================================"
  echo "Starting Emacs..."
  echo "Please wait for packages to install, then:"
  echo "1. Run: M-x elpaca-log-updates (to verify installation)"
  echo "2. Run: (elpaca-write-lockfile) in *scratch* buffer"
  echo "3. Quit Emacs when done"
  echo "======================================"
  
  # Launch Emacs and wait for user to quit
  emacs
  
  # After Emacs exits, move lockfile and commit
  if [ -f ${HOME}/.config/emacs/elpaca.lock ]; then
    mv ${HOME}/.config/emacs/elpaca.lock .
    git add elpaca.lock
    echo "Lock file updated and staged successfully!"
  else
    echo "ERROR: Lock file not found at ${HOME}/.config/emacs/elpaca.lock"
    echo "Did you run (elpaca-write-lockfile)?"
    exit 1
  fi
