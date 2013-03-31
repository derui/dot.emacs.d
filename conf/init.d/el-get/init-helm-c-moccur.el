(require 'helm-c-moccur)

;; all.elとの連携を行う
(defun all-from-helm-moccur ()
  "Call `all' from `helm' content."
  (interactive)
  (helm-run-after-quit
   'all-from-anything-occur-internal "helm-moccur"
   helm-c-moccur-buffer helm-current-buffer))

(define-key helm-c-moccur-helm-map (kbd "C-c C-a")
  'all-from-helm-moccur)
