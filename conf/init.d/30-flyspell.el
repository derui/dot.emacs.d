(eval-when-compile
  (require 'use-package))

(require 'flyspell)
(require 'ispell)

(defvar my:ispell-regexp-ja "[一-龠ぁ-🈀ァ-𛀀ー・、。々]+"
  "Regular expression to match a Japanese word.
The expression can be [^\000-\377]+, [^!-~]+, or [一-龠ぁ-🈀ァ-𛀀ー・、。々]+")

(defun my:flyspell-skip-ja (beg end info)
  "Tell flyspell to skip a Japanese word.
Call this on `flyspell-incorrect-hook'."
  (string-match my:ispell-regexp-ja (buffer-substring beg end)))

(defun my:flyspell-enable ()
  "The function to enable flyspell in current buffer."
  (interactive)
  (flyspell-mode 1))

;; Use hunspell instead of ispell/aspell
(when (executable-find "hunspell")
  (setq flyspell-default-dictionary "en_US")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)



  ;; for performance
  (setq flyspell-issue-message-flag nil)

  (add-hook 'flyspell-incorrect-hook #'my:flyspell-skip-ja))

;;; configurations for LanguageTool to check english grammer.
(defvar my:langtool-version "4.2")
(defvar my:langtool-cli-path (expand-file-name (locate-user-emacs-file
                                                (format "share/LanguageTool-%s/languagetool-commandline.jar"
                                                        my:langtool-version))))

;; setup languagetool-commandline
(unless (file-exists-p my:langtool-cli-path)
  (when (eq window-system 'x)

    (make-directory (expand-file-name "~/.emacs.d/share") t)
    (let ((langtool-url (format "https://languagetool.org/download/LanguageTool-%s.zip" my:langtool-version))
          (output "/tmp/LanguageTool.zip"))

      (call-process "curl" nil nil t "-L" "-o" output langtool-url)
      (call-process "unzip" nil nil t "-d" (expand-file-name "~/.emacs.d/share") output)
      (rename-file (format "~/.emacs.d/share/LanguageTool-%s/languagetool-commandline.jar" my:langtool-version)
                   my:langtool-cli-path t))))

(use-package popup :commands (popup-tip))
(use-package langtool
  :after (popup)
  :commands (langtool-details-error-message)
  :custom
  (langtool-language-tool-jar my:langtool-cli-path)
  (langtool-default-language "en-US")
  (langtool-java-user-arguments '("-Dfile.encoding=UTF-8"))
  :preface
  (defun my:langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g' .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  :config
  (setq langtool-autoshow-message-function #'my:langtool-autoshow-detail-popup))
