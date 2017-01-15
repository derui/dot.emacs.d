;;; IME(SKK, sekka, mozc...)についての設定を記述する。

;; (@* "Mozcについての設定")
(when (boundp 'my:mozc-helper-locate)
  (require 'mozc)
  (setq mozc-candidate-style 'echo-area)
  (defvar my:input-method "japanese-mozc")
  (setq-default default-input-method my:input-method)
  (setq default-input-method my:input-method)
  (setq mozc-helper-program-name my:mozc-helper-locate)

  (require 'popup)

  ;; popup スタイルを使用
  (setq mozc-candidate-style 'popup)

  (push '(popup
          (clean-up . mozc-cand-popup-clean-up)
          (clear . mozc-cand-popup-clear)
          (update . mozc-cand-popup-update))
        mozc-candidate-dispatch-table)

  (defvar-local mozc-cand-popup nil)

  (defconst mozc-cand-popup-shortcut-spacer ". ")
  (defconst mozc-cand-popup-description-space 3)

  (defun mozc-cand-popup-update (candidates)
    (let* ((focused-index (mozc-protobuf-get candidates 'focused-index))
           (candidates-size (mozc-protobuf-get candidates 'size))
           (footer-label (or (mozc-protobuf-get candidates 'footer 'label) " "))
           (index-visible (mozc-protobuf-get candidates 'footer 'index-visible))
           (max-width (string-width footer-label))
           (items (mapcar
                   (lambda (candidate)
                     (let ((index (mozc-protobuf-get candidate 'index))
                           (value (mozc-protobuf-get candidate 'value))
                           (description (mozc-protobuf-get candidate 'annotation 'description))
                           (shortcut (mozc-protobuf-get candidate 'annotation 'shortcut)))
                       (setq max-width (max (+ (string-width value)
                                               (if shortcut
                                                   (+ (string-width mozc-cand-popup-shortcut-spacer)
                                                      (string-width shortcut)) 0)
                                               (if description
                                                   (+ mozc-cand-popup-description-space
                                                      (string-width description)) 0))
                                            max-width))
                       (popup-make-item (if shortcut
                                            (concat shortcut
                                                    mozc-cand-popup-shortcut-spacer
                                                    value)
                                          value)
                                        :face (if (zerop (logand index 1))
                                                  'mozc-cand-overlay-even-face
                                                'mozc-cand-overlay-odd-face)
                                        :summary description
                                        )))
                   (cdr (assq 'candidate candidates)))))
      (add-to-list
       'items
       (popup-make-item footer-label
                        :face 'mozc-cand-overlay-footer-face
                        :summary (when (and index-visible focused-index candidates-size)
                                   (format "%d/%d" (1+ focused-index) candidates-size)))
       t)

      (mozc-cand-popup-clear)
      (setq mozc-cand-popup (popup-create
                             mozc-preedit-point-origin
                             max-width (length items)
                             :around t
                             :margin-left 1
                             :margin-right 1
                             :selection-face (if focused-index
                                                 'mozc-cand-overlay-focused-face
                                               'mozc-cand-overlay-footer-face)
                             :summary-face 'mozc-cand-overlay-footer-face))
      (popup-set-list mozc-cand-popup items)
      (if focused-index
          (popup-select mozc-cand-popup (% focused-index 9))
        ;; when not focused, select footer at once.
        (popup-select mozc-cand-popup (1- (length items))))
      (popup-draw mozc-cand-popup)
      ))

  (defun mozc-cand-popup-clear ()
    (popup-delete mozc-cand-popup))

  (defun mozc-cand-popup-clean-up ()
    (mozc-cand-popup-clear))

  (defun my:enable-mozc ()
    (interactive)
    (set-input-method my:input-method))

  (defun my:disable-mozc ()
    (interactive)
    (set-input-method nil))

  (global-set-key (kbd "<Hangul>") #'my:enable-mozc)
  (global-set-key (kbd "<henkan>") #'my:enable-mozc)
  (global-set-key (kbd "<Hangul_Hanja>") #'my:disable-mozc)
  (global-set-key (kbd "<muhenkan>") #'my:disable-mozc))
