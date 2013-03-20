;;; pomodoro.el --- Pomodoro Technique in Emacs

;; Author: Syohei Yoshida(syohex@gmail.com)
;; Version: 0.01

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup pomodoro nil
  "Pomodoro in Emacs"
  :prefix "pomodoro:"
  :group 'pomodoro)

(defcustom pomodoro:file "~/.emacs.d/pomodoro.org"
  "Pomodoro check file"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro:work-time 25
  "Work minitus"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:rest-time 5
  "Rest minutes"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:long-rest-time 30
  "Rest minutes"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:iteration-for-long-rest 4
  "Iteration count when switching to long rest"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:max-iteration 0
  "Iteration count to stop pomodoro"
  :group 'pomodoro
  :type 'integer)

(defcustom pomodoro:mode-line-work-sign "●"
  "String in mode line at work"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro:mode-line-rest-sign "●"
  "String in mode line at rest"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro:mode-line-long-rest-sign "●"
  "String in mode line at long rest"
  :group 'pomodoro
  :type 'string)

(defcustom pomodoro:mode-line-time-display t
  "Flag to display remaining time in the mode line."
  :type 'boolean
  :group 'pomodoro)

(defface pomodoro:work-face
  '((t (:foreground "red")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro:rest-face
  '((t (:foreground "blue")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro:long-rest-face
  '((t (:foreground "green")))
  "mode-line-face"
  :group 'pomodoro)

(defface pomodoro:timer-face
  '((t (:weight bold)))
  "mode-line-face"
  :group 'pomodoro)

(defvar pomodoro:timer nil)

(defvar pomodoro:work-count 0)

(defvar pomodoro:current-state 'working
  "Pomodoro statement flag, working or rest")

(defvar pomodoro:remainder-seconds 0)

(defvar pomodoro:finish-work-hook nil)
(defvar pomodoro:finish-rest-hook nil)
(defvar pomodoro:long-rest-hook nil)

(defmacro pomodoro:set-state (mode)
  `(setq pomodoro:current-state ,mode))

(defmacro pomodoro:reset-remainder-time (time)
  `(setq pomodoro:remainder-seconds (* ,time 60)))

(defun pomodoro:switch-to-long-rest ()
  (pomodoro:set-state 'long-rest)
  (run-hooks 'pomodoro:long-rest-hook)
  (pomodoro:reset-remainder-time pomodoro:long-rest-time))

(defun pomodoro:long-rest-p ()
  (zerop (mod pomodoro:work-count pomodoro:iteration-for-long-rest)))

(defun pomodoro:switch-to-rest ()
  (pomodoro:set-state 'rest)
  (when pomodoro:file
    (find-file pomodoro:file))
  (incf pomodoro:work-count)
  (message "%d time(s) pomodoro!" pomodoro:work-count)
  (cond ((pomodoro:long-rest-p)
         (pomodoro:switch-to-long-rest))
        (t
         (run-hooks 'pomodoro:finish-work-hook)
         (pomodoro:reset-remainder-time pomodoro:rest-time))))

(defun pomodoro:switch-to-work ()
  (pomodoro:set-state 'working)
  (run-hooks 'pomodoro:finish-rest-hook)
  (pomodoro:reset-remainder-time pomodoro:work-time))

(defvar pomodoro:mode-line "")

(defun pomodoro:time-to-string (seconds)
  (format "%02d:%02d" (/ seconds 60) (mod seconds 60)))

(defun pomodoro:propertize-sign ()
  (cond ((eq pomodoro:current-state 'rest)
         (propertize pomodoro:mode-line-rest-sign 'face 'pomodoro:rest-face))
        ((eq pomodoro:current-state 'long-rest)
         (propertize pomodoro:mode-line-long-rest-sign 'face
                     'pomodoro:long-rest-face))
        ((eq pomodoro:current-state 'working)
         (propertize pomodoro:mode-line-work-sign 'face 'pomodoro:work-face))
        (t nil)))

(defun pomodoro:propertize-mode-line ()
  (unless (string= pomodoro:mode-line "")
    (if pomodoro:mode-line-time-display
        (concat (pomodoro:propertize-sign)
                (propertize pomodoro:mode-line 'face 'pomodoro:timer-face))
      (pomodoro:propertize-sign))))

(defun pomodoro:set-mode-line ()
  (setq pomodoro:mode-line
        (pomodoro:time-to-string pomodoro:remainder-seconds)))

(defun pomodoro:expire ()
  (cond ((or (eq pomodoro:current-state 'rest)
             (eq pomodoro:current-state 'long-rest))
         (pomodoro:switch-to-work))
        ((and (not (eq pomodoro:max-iteration 0))
              (<= pomodoro:max-iteration pomodoro:work-count))
         (run-with-timer 0 nil 'pomodoro:stop))
        (t
         (pomodoro:switch-to-rest))))

(defun pomodoro:tick ()
  (let ((remainder-seconds (1- pomodoro:remainder-seconds)))
    (if (< remainder-seconds 0)
        (pomodoro:expire)
      (decf pomodoro:remainder-seconds))
    (pomodoro:set-mode-line)
    (pomodoro:propertize-mode-line)
    (force-mode-line-update)))

(defun pomodoro:set-remainder-second (minutes)
  (setq pomodoro:remainder-seconds (* 60 minutes)))

(defun pomodoro:clear-mode-line ()
  (setq pomodoro:mode-line "")
  (force-mode-line-update))

(defun pomodoro:current-time-to-string ()
  (format-time-string "%m:%d" (current-time)))

(defvar pomodoro:last-work-time nil
  "Last time of pomodoro work(format 'Month:Day')")

(defun pomodoro:last-work-today-p ()
  (or (not pomodoro:last-work-time)
      (string= pomodoro:last-work-time (pomodoro:current-time-to-string))))

(defun pomodoro:today-work-count ()
  (interactive)
  (message "Today's Pomodoro Count is %d !!" pomodoro:work-count))

(defun pomodoro:reset-work-count ()
  (interactive)
  (setq pomodoro:work-count 0)
  (message "Reset the work-count %d." pomodoro:work-count))

;;;###autoload
(defun pomodoro:start (minutes)
  (interactive "P")
  (when pomodoro:timer
    (error "Already start timer!!"))
  (when (consp current-prefix-arg)
    (setq minutes
          (string-to-number (read-string "How long pomodoro minutes >> "))))
  (when (not (pomodoro:last-work-today-p))
    (message "Reset Pomodoro Count")
    (setq pomodoro:work-count 0))
  (setq pomodoro:last-work-time (pomodoro:current-time-to-string))
  (pomodoro:set-state 'working)
  (pomodoro:set-remainder-second (or minutes pomodoro:work-time))
  (setq pomodoro:timer (run-with-timer 0 1 'pomodoro:tick)))

(defun pomodoro:stop (&optional do-reset)
  (interactive)
  (pomodoro:set-state nil)
  (when do-reset
    (setq pomodoro:work-count 0))
  (cancel-timer pomodoro:timer)
  (setq pomodoro:timer 'nil)
  (pomodoro:clear-mode-line))

(defun pomodoro:reset ()
  (interactive)
  (pomodoro:stop t))

(defun pomodoro:mode-line-time-display-toggle ()
  "Toggle remaining time"
  (interactive)
  (setq pomodoro:mode-line-time-display (not pomodoro:mode-line-time-display)))

(defvar pomodoro:set-mode-line-p nil)

(unless pomodoro:set-mode-line-p
  (setq-default mode-line-format
                (cons '(:eval (pomodoro:propertize-mode-line))
                      mode-line-format))
  (setq pomodoro:set-mode-line-p t))

(provide 'pomodoro)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; pomodoro.el ends here
