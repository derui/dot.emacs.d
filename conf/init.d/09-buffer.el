;; 各種バッファーに関連する設定

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("\*omake\*.*" :noselect t :regexp t))

(require 'powerline)
(powerline-default-theme)

(defun get-buffer-file-eol-type ()
  (case (coding-system-eol-type buffer-file-coding-system)
    (0 "LF")
    (1 "CRLF")
    (2 "CR")
    (otherwise "??")))

(defun get-buffer-coding-type-without-eol-type ()
  (cl-labels
      ((remove-os-info (string)
                       (replace-regexp-in-string "-\\(dos\\|unix\\|mac\\)$" "" string)))
    (lexical-let
        ((string
          (replace-regexp-in-string "-with-signature" "(bom)"
                                    (remove-os-info  (symbol-name buffer-file-coding-system)))))
      (if (string-match-p "(bom)" string)
          (downcase string)
        (upcase string)))))

(defface powerline-active3
  '((t (:background "Springgreen4" :inherit mode-line-inactive
                    :foreground "white")))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-inactive3
  '((t (:background "grey0" :inherit mode-line-inactive)))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-active4
  '((t (:background "VioletRed" :inherit mode-line-inactive
                    :foreground "white")))
  "Powerline face 4."
  :group 'powerline)

(defface powerline-inactive4
  '((t (:background "grey0" :inherit mode-line-inactive)))
  "Powerline face 4."
  :group 'powerline)

(defface powerline-active5
  '((t (:background "Yellow4" :inherit mode-line-inactive
                    :foreground "white")))
  "Powerline face 5."
  :group 'powerline)

(defface powerline-inactive5
  '((t (:background "grey0" :inherit mode-line-inactive)))
  "Powerline face 5."
  :group 'powerline)

(defface powerline-active6
  '((t (:background "white" :inherit mode-line-inactive)))
  "Powerline face 6."
  :group 'powerline)

(defface powerline-inactive6
  '((t (:background "grey0" :inherit mode-line-inactive)))
  "Powerline face 6."
  :group 'powerline)

(defpowerline powerline-coding-type
  (concat (get-buffer-coding-type-without-eol-type) "[" (get-buffer-file-eol-type) "]"))

(defpowerline powerline-buffer-status
  (concat (if buffer-read-only "r-" "rw")
          ":"
          (if (buffer-modified-p) "*" "-")))


(defun powerline-my-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 (if active 'powerline-active3 'powerline-inactive3))
                          (face4 (if active 'powerline-active4 'powerline-inactive4))
                          (face5 (if active 'powerline-active5 'powerline-inactive5))
                          (face6 (if active 'powerline-active6 'powerline-inactive6))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
                                ;; (powerline-raw "%*" nil 'l)
                                (powerline-coding-type nil 'l)
                                (powerline-buffer-status nil 'l)
                                ;; (powerline-buffer-size nil 'l)
                                (funcall separator-left face1 face6)
                                (powerline-buffer-id face5 'l)
                                ;; (powerline-raw " ")
                                (funcall separator-left face6 face1)
                                (powerline-major-mode face1 'l)
                                (powerline-narrow face1 'l)
                                (powerline-vc face1)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw "%4l" face1 'r)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right nil face5)
                                     ;; (powerline-raw " ")
                                     (powerline-raw "%6p" face5 'r)
                                     (powerline-hud face2 face1)))
                          (center (list (powerline-raw " " face1)
                                        (funcall separator-left face1 face2)
                                        (when (boundp 'erc-modified-channels-object)
                                          (powerline-raw erc-modified-channels-object face2 'l))
                                        (powerline-process face2)
                                        ;; (powerline-raw " :" face2)
                                        (powerline-minor-modes face2 'l)
                                        (powerline-raw " " face2)
                                        (funcall separator-right face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))



(powerline-my-theme)

