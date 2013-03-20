;;; 各種設定ファイルを読み込む。
;;; 読み込む順番は、init-loaderに関係なく、このファイルで設定する。
;;; init-loader.elを使うほどでも無いので、自前で似たようなものを
;;; 作成して利用する。

(defvar my:init-file-directory (locate-user-emacs-file "conf/init.d"))

(defvar my:init-loader-log-buffer-name "*my:init-loader log*")

(defun my:init-loader-log (log)
  (save-excursion
    (with-current-buffer (get-buffer-create my:init-loader-log-buffer-name)
      (insert (concat log "\n")))))

(defun my:byte-compile-file-p (file)
  (let* ((elc-file (concat (file-name-nondirectory file) "c"))
         (elc-file (concat (file-name-as-directory my:init-file-directory) elc-file)))
    (if (file-exists-p elc-file)
        (let* ((get-m-time '(lambda (file) (nth 5 (file-attributes file))))
               (elc-mtime (float-time (funcall get-m-time elc-file)))
               (file-mtime (float-time (funcall get-m-time file))))
          (< elc-mtime file-mtime))
      t)))

(defun my:load-file (file)
  (let* ((elc-file (concat (file-name-nondirectory file) "c"))
         (elc-file (concat (file-name-as-directory my:init-file-directory) elc-file)))
    (if (my:byte-compile-file-p file)
        (progn
          (load-file file)
          (byte-compile-file file))
      (load-file file))))

(defun my:init-loader-load (base-dir)

  (let ((load-path (cons base-dir load-path)))
    (let* ((files (directory-files base-dir nil ".+\.el$"))
           (sorted (sort (remove ".." (remove "." files)) 'string<)))
      ;; どこでエラーになったかわからなくなるので、読み込む前後でログを出しておく
      (dolist (file sorted)
        (condition-case e
            (progn 
              (my:init-loader-log (format "start load init file : %s" file))
              (my:load-file (concat (file-name-as-directory my:init-file-directory) file))
              (my:init-loader-log (format "end   load init file : %s" file)))
          (error
           (my:init-loader-log (format "%s . %s" file e))))
        ))))

(my:init-loader-load my:init-file-directory)
