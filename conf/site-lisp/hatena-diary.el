(require 'tabulated-list)
(require 'hatena-diary-api)
(eval-when-compile (require 'cl))

(defgroup hatena-diary nil
  "Access to Hatena::Diary."
  :group 'applications)

(defcustom hatena:d:username nil
  "User name for Hatena::Diary.
This can be different from `hatena:username', which is used for
authentication together with `hatena:password'."
  :type '(choice string (const nil :tag "none"))
  :group 'hatena-diary)

(defcustom hatena:d:auto-pager t
  "t means that the next page of entry list is automatically
loaded when the cursor reaches to the end of the buffer."
  :type 'boolean
  :group 'hatena-diary)

(defcustom hatena:d:major-mode 'html-mode
  "Major mode for editing a Hatena::Diary entry."
  :type 'function
  :group 'hatena-diary)

(defcustom hatena:d:no-auto-save t
  "t means to disable auto-saving Hatena::Diary entries."
  :type 'boolean
  :group 'hatena-diary)

(defcustom hatena:d:working-directory
  (concat (file-name-as-directory user-emacs-directory) "hatena/")
  "Directory used by Hatena::Diary application.
Hatena::Diary uses this directory only for auto-saving buffer
contents."
  :type 'directory
  :group 'hatena-diary)

(defface hatena:d:list-delete
  '((t (:inherit error)))
  "Face for list item marked as to delete."
  :group 'hatena-diary)

(defface hatena:d:list-publish
  '((t (:inherit font-lock-constant-face)))
  "Face for list item marked as to publish."
  :group 'hatena-diary)

(defconst hatena-d-entries-buffer "*Hatena::Diary Entries%s*")
(defconst hatena-d-drafts-buffer "*Hatena::Diary Drafts%s*")
(defconst hatena-d-preview-buffer "*Hatena::Diary Preview%s*")
(defconst hatena-d-title-width 56)
(defconst hatena-d-date-width 20)
(defconst hatena-d-category-width 20)
(defconst hatena-d-list-message
  "Commands: N, v, V, c, C; d, P, u, x; q to quit; ? for help.")

(defvar hatena-d-entry nil)
(make-variable-buffer-local 'hatena-d-entry)
(put 'hatena-d-entry 'permanent-local t)
(defvar hatena-d-file-name nil)
(make-variable-buffer-local 'hatena-d-file-name)
(put 'hatena-d-file-name 'permanent-local t)
(defvar hatena-d-type nil)
(make-variable-buffer-local 'hatena-d-type)
(defvar hatena-d-current-page nil)
(make-variable-buffer-local 'hatena-d-current-page)

(defvar hatena:d:list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'hatena:d:list-select)
    (define-key map (kbd "v") #'hatena:d:list-preview)
    (define-key map (kbd "V") #'hatena:d:list-view)
    (define-key map (kbd "d") #'hatena:d:list-delete)
    (define-key map (kbd "D") #'hatena:d:list-delete)
    (define-key map (kbd "P") #'hatena:d:list-publish)
    (define-key map (kbd "u") #'hatena:d:list-unmark)
    (define-key map (kbd "x") #'hatena:d:list-execute)
    (define-key map (kbd "N") #'hatena:d:list-retrieve-next)
    (define-key map (kbd "c") #'hatena:d:new)
    (define-key map (kbd "C") #'hatena:d:new-draft)
    map))

(defvar hatena:d:list-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'hatena:d:list-mouse-select)
    map))

(defsubst hatena:d:user ()
  (or hatena:d:username (hatena:username)))

(defsubst hatena-d-entry-file (entry)
  (concat (file-name-as-directory hatena:d:working-directory)
          (hatena-d-entry-unique-name entry)))

(defsubst hatena-d-format-time (time)
  (format-time-string "%Y-%m-%d %T" time))

(defsubst hatena-d-buffer-name (base user)
  (format base (or (and user (format " [%s]" user)) "")))

(defsubst hatena-d-entries-buffer (&optional user)
  (hatena-d-buffer-name hatena-d-entries-buffer user))

(defsubst hatena-d-drafts-buffer (&optional user)
  (hatena-d-buffer-name hatena-d-drafts-buffer user))

(defsubst hatena-d-preview-buffer (&optional user)
  (hatena-d-buffer-name hatena-d-preview-buffer user))

(defsubst hatena-d-list-get-cols ()
  (or (get-text-property (point) 'tabulated-list-entry)
      (car-safe (cdr (assq (tabulated-list-get-id) tabulated-list-entries)))))

;; commands

;;;###autoload
(defun hatena:d:list (&optional arg)
  "List Hatena::Diary blog entries in a buffer."
  (interactive "P")
  (switch-to-buffer (hatena:d:list-noselect arg))
  (message hatena-d-list-message))

;;;###autoload
(defun hatena:d:list-noselect (&optional arg)
  "List Hatena::Diary blog entries in a buffer without selecting it."
  (interactive "P")
  (let* ((user (and arg (hatena-d-list-ask-user)))
         (buffer (get-buffer-create (hatena-d-entries-buffer user))))
    (with-current-buffer buffer
      (hatena:d:list-mode)
      (hatena-d-list-set-user user)
      (hatena-d-list-refresh)
      (tabulated-list-print))
    buffer))

;;;###autoload
(defun hatena:d:list-draft (&optional arg)
  "List Hatena::Diary draft entries in a buffer."
  (interactive "P")
  (switch-to-buffer (hatena:d:list-draft-noselect arg))
  (message hatena-d-list-message))

;;;###autoload
(defun hatena:d:list-draft-noselect (&optional arg)
  "List Hatena::Diary draft entries in a buffer without selecting it."
  (interactive "P")
  (let* ((user (and arg (hatena-d-list-ask-user)))
         (buffer (get-buffer-create (hatena-d-drafts-buffer user))))
    (with-current-buffer buffer
      (hatena:d:list-mode)
      (hatena-d-list-set-user user)
      (hatena-d-list-refresh 'draft)
      (tabulated-list-print))
    buffer))

;;;###autoload
(defun hatena:d:new-noselect (&optional entry buf)
  "Open a buffer for a Hatena::Diary entry without selecting the buffer.
If ENTRY is specified, open a buffer for the entry. Otherwise,
open a buffer for a new entry."
  (interactive)
  (unless entry
    (setq entry (hatena:d:make-entry :type 'blog :user (hatena:d:user))))
  (unless (hatena:d:entry-p entry) (error "Non entry object specified"))
  (let* ((title (or (hatena:d:entry-title entry) " Title"))
         (content (or (hatena:d:api:get-source entry) ""))
         (name (hatena-d-entry-unique-name entry))
         (buf (or buf (get-buffer-create name))))
    (with-current-buffer buf
      (when (or (hatena:d:entry-id entry) (= (point-min) (point-max)))
        ;; initialize
        (erase-buffer)
        (insert (format "*%s\n\n" title))
        (insert content)
        (unless (= (char-before (point)) ?\n) (insert "\n"))
        (when (fboundp hatena:d:major-mode) (funcall hatena:d:major-mode))
        (goto-char (point-min))
        (when hatena:d:no-auto-save
          (auto-save-mode -1)
          (set (make-local-variable 'auto-save-default) nil))
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil))
      (setq hatena-d-entry entry)
      (hatena:d:edit-mode 1)
      buf)))

;;;###autoload
(defun hatena:d:new (&optional entry)
  "Open a buffer for a Hatena::Diary entry.
If ENTRY is specified, open a buffer for the entry. Otherwise,
open a buffer for a new entry."
  (interactive)
  (switch-to-buffer (hatena:d:new-noselect entry)))

;;;###autoload
(defun hatena:d:new-draft-noselect ()
  "Open a buffer for a new Hatena::Diary draft without selecting the buffer."
  (interactive)
  (hatena:d:new (hatena:d:make-entry :type 'draft :user (hatena:d:user))))

;;;###autoload
(defun hatena:d:new-draft ()
  "Open a buffer for a new Hatena::Diary draft."
  (interactive)
  (switch-to-buffer (hatena:d:new-draft-noselect)))

;; modes

(define-minor-mode hatena:d:edit-mode
  "Minor mode for editing Hatena::Diary entry."
  :group 'hatena-diary
  (if hatena:d:edit-mode
      ;; on
      (let ((name (hatena-d-entry-file hatena-d-entry)))
        (hatena-d-set-file-name name)
        (set-buffer-modified-p nil)
        (hatena-d-set-revert-function)
        (add-hook 'multi-indirect-buffer-hook
                  #'hatena-d-set-revert-function nil t)
        (when (boundp 'multi-indirect-buffers-alist)
          (dolist (elt multi-indirect-buffers-alist)
            (with-current-buffer (cdr elt)
              (unless (null (buffer-base-buffer))
                (hatena-d-set-revert-function)))))
        (add-hook 'write-file-functions #'hatena:d:save nil t)
        (add-hook 'after-change-major-mode-hook
                  #'hatena:d:restore-edit-mode nil t))
    ;; off
    (kill-local-variable 'revert-buffer-function)
    (remove-hook 'after-change-major-mode-hook #'hatena:d:restore-edit-mode t)
    (remove-hook 'write-contents-functions #'hatena:d:save t)))

(define-derived-mode hatena:d:list-mode tabulated-list-mode
  "Hatena::Diary"
  "Major mode for listing Hatena::Diary entries.
The entry list is invoked by the commands \\[hatena:d:list] and
\\[hatena:d:list-draft].

In Hatena::Diary entry list mode, the following commands are defined:
\\<hatena:d:list-mode-map>
\\[quit-window]    Remove the entry list from the display.
\\[hatena:d:list-select]  Select this line's entry and open its source text.
\\[hatena:d:list-preview]    Toggle preview mode.
\\[hatena:d:list-view]    Select this line's entry and open its source text in `view-mode'.
\\[hatena:d:list-delete]    Mark the buffer on this entry line for deletion.
\\[hatena:d:list-publish]    Mark the buffer on this draft entry line for publish.
\\[hatena:d:list-unmark]    Cancel all requested operations on buffer on this line.
\\[hatena:d:list-execute]    Do marked operations.
\\[hatena:d:list-retrieve-next]    Retrieve the next page of entries and add them to the list.
\\[hatena:d:new]    Open a buffer for a new blog entry.
\\[hatena:d:new-draft]    Open a buffer for a new draft entry."
  (add-hook 'tabulated-list-revert-hook #'hatena-d-list-refresh nil t)
  (add-hook 'post-command-hook #'hatena-d-list-auto-pager nil t)
  (setq show-trailing-whitespace nil)
  (hl-line-mode 1))

(define-minor-mode hatena:d:list-preview-mode
  "Minor mode for Hatena::Diary list preview."
  :group 'hatena-diary
  (if hatena:d:list-preview-mode
      ;; on
      (add-hook 'post-command-hook #'hatena:d:list-preview1 nil t)
    ;; off
    (remove-hook 'post-command-hook #'hatena:d:list-preview1 t)
    (hatena:d:list-close-preview)))

;; mode commands

(defun hatena:d:list-select ()
  "Select this line's entry and open its source text."
  (interactive)
  (hatena:d:list-close-preview)
  (let ((entry (tabulated-list-get-id)))
    (when (hatena:d:entry-p entry) (hatena:d:new entry))))

(defun hatena:d:list-mouse-select (event)
  "Select the entry whose line you click on and open its source text."
  (interactive "e")
  (hatena:d:list-close-preview)
  (select-window (posn-window (event-end event)))
  (let ((entry (tabulated-list-get-id (posn-point (event-end event)))))
    (when (hatena:d:entry-p entry) (hatena:d:new entry))))

(defun hatena:d:list-view ()
  "Select this line's entry and open its source text in `view-mode'."
  (interactive)
  (hatena:d:list-close-preview)
  (let ((entry (tabulated-list-get-id)))
    (when (hatena:d:entry-p entry)
      (let ((buf (hatena:d:new-noselect entry)))
        (with-current-buffer buf
          (view-mode 1))
        (switch-to-buffer buf)))))

(defun hatena:d:list-preview1 ()
  (let ((entry (tabulated-list-get-id))
        (win (selected-window))
        (buf (get-buffer-create (hatena-d-preview-buffer hatena:d:username))))
    (if (not (hatena:d:entry-p entry))
        (hatena:d:list-close-preview)
      (with-current-buffer buf
        (setq show-trailing-whitespace nil)
        (erase-buffer)
        (insert (hatena:d:entry-content entry))
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (cond
         ((eq (hatena:d:entry-type entry) 'blog)
          (if (fboundp 'w3m-buffer) (w3m-buffer) (html-mode)))
         ((eq (hatena:d:entry-type entry) 'draft)
          (when (fboundp hatena:d:major-mode) (funcall hatena:d:major-mode)))))
      (switch-to-buffer-other-window buf t)
      (select-window win))))

(defun hatena:d:list-preview ()
  "Toggle preview mode.
A preview of this line's entry shows up on the other buffer.

For blog entries, the preview is either formatted text generated
by `w3m' or `html-mode'. For draft entries, the preview is a
source text with `hatena:d:major-mode'."
  (interactive)
  (if hatena:d:list-preview-mode ; toggle
      (hatena:d:list-preview-mode 0)
    (hatena:d:list-preview-mode 1)
    (hatena:d:list-preview1)))

(defun hatena:d:list-close-preview ()
  (let ((win (get-buffer-window (hatena-d-preview-buffer hatena:d:username))))
    (when win (delete-window win))))

(defun hatena:d:list-delete ()
  "Mark the buffer on this entry line for deletion."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (error "No entry at point")))
  (hatena-d-list-put-mark "D")
  (hatena-d-list-apply-face 'hatena:d:list-delete)
  (forward-line 1))

(defun hatena:d:list-publish ()
  "Mark the buffer on this draft entry line for publish."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (error "No entry at point"))
    (unless (and (hatena:d:entry-p entry)
                 (eq (hatena:d:entry-type entry) 'draft))
      (error "No draft entry at point")))
  (hatena-d-list-put-mark "P")
  (hatena-d-list-apply-face 'hatena:d:list-publish)
  (forward-line 1))

(defun hatena:d:list-unmark (&optional noforward)
  "Cancel all requested operations on buffer on this line."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry (error "No entry at point")))
  (hatena-d-list-put-mark " ")
  (hatena-d-list-apply-face 'default)
  (unless noforward (forward-line 1)))

(defun hatena:d:list-execute1 (entry what)
  (let (status)
    (cond
     ((= what ?D)
      (setq status (hatena:d:api:delete entry)))
     ((= what ?P)
      (setq status (hatena:d:api:publish entry))))
    (when status
      (message status)
      (string-match-p "200 OK$" status))))

(defun hatena:d:list-execute ()
  "Do marked operations."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((entry (tabulated-list-get-id)))
        (cond
         ((hatena:d:entry-p entry)
          (if (prog1 (hatena:d:list-execute1 entry (char-after))
                (hatena:d:list-unmark t))
              (hatena-d-list-delete)
            (forward-line 1)))
         (t (forward-line 1)))))))

(defun hatena:d:list-retrieve-next ()
  "Retrieve the next page of entries and add them to the list."
  (interactive)
  (let* ((type (or hatena-d-type 'blog))
         (user (hatena:d:user))
         (page (1+ (or hatena-d-current-page 0)))
         (es (hatena:d:api:entries type user page)))
    (setq hatena-d-current-page page)
    (hatena-d-list-append es)
    (tabulated-list-print t)))

(defun hatena:d:save (&optional entry)
  "Save the Hatena::Diary entry.
If the entry is a blog entry, it is immediately reflected to the
public Web page of Hatena::Diary. If the entry is a draft entry,
it is saved to the draft collection."
  (interactive)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((entry (or entry hatena-d-entry)) status)
      (unless (hatena:d:entry-p entry)
        (error "This buffer is not a Hatena::Diary entry."))
      (destructuring-bind (title content) (hatena-d-parse-content)
        (setf (hatena:d:entry-title entry) title)
        (setf (hatena:d:entry-source entry) content)
        (setq status (hatena:d:api:save entry))
        (message status)
        (when (or (string-match-p "200 OK$" status)
                  (string-match-p "201 Created$" status))
          (setq hatena-d-entry entry)
          (setq last-coding-system-used 'utf-8)
          (delete-auto-save-file-if-necessary)
          (hatena:d:edit-mode 1)
          t)))))

(defun hatena:d:save-as-draft ()
  "Save current entry as a draft.
If the current entry is already a draft, then it is saved as
usual. Otherwise, if the current entry is a blog entry, then the
contents of the entry is saved as a new draft entry."
  (interactive)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((entry hatena-d-entry))
      (if (eq (hatena:d:entry-type entry) 'draft)
          (hatena:d:save)
        (let* ((user (hatena:d:entry-user entry))
               (entry (hatena:d:make-entry :type 'draft :user user)))
          (hatena:d:save entry))))))

(defun hatena:d:revert (ignore1 ignore2)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((mod (buffer-modified-p)))
      (set-buffer-modified-p nil)
      (when (fboundp 'multi-mode-quit) (multi-mode-quit))
      (set-buffer-modified-p mod))
    (hatena:d:new-noselect hatena-d-entry (current-buffer))))

(defun hatena:d:restore-edit-mode (&rest ignore)
  "Re-enable `hatena:d:edit-mode'."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (when (and hatena-d-file-name
               (string= (expand-file-name (buffer-file-name))
                        (expand-file-name hatena-d-file-name))
               (or (and (numberp hatena:d:edit-mode)
                        (<= hatena:d:edit-mode 0))
                   (null hatena:d:edit-mode)))
      (hatena:d:edit-mode 1))))
(put 'hatena:d:restore-edit-mode 'permanent-local-hook t)

;; internal functions

(defun hatena-d-list-ask-user ()
  (read-string "User: " (hatena:d:user)))

(defun hatena-d-list-set-user (user)
  (when user (set (make-local-variable 'hatena:d:username) user)))

(defun hatena-d-list-title (entry)
  (let ((title (hatena:d:entry-title entry)))
    (propertize
     (replace-regexp-in-string "^\\(\\[.*?\\]\\)*[ \t\r\n]*" "" title)
     'mouse-face 'highlight
     'local-map hatena:d:list-item-map)))

(defun hatena-d-list-created (entry)
  (hatena-d-format-time (hatena:d:entry-created entry)))

(defun hatena-d-list-category (entry)
  (let ((title (hatena:d:entry-title entry))
        (regexp "^\\[\\(.*?\\)\\]")
        categories)
    (while (string-match regexp title)
      (push (match-string 1 title) categories)
      (setq title (replace-regexp-in-string regexp "" title)))
    (mapconcat #'identity (sort categories #'string<) ",")))

(defun hatena-d-list-append (entries &optional initialize)
  (when initialize (setq tabulated-list-entries nil))
  (setq tabulated-list-entries
        (append tabulated-list-entries
                (mapcar
                 #'(lambda (entry)
                     (let ((title (hatena-d-list-title entry))
                           (created (hatena-d-list-created entry))
                           (categories (hatena-d-list-category entry)))
                       (list entry (vector "" title created categories))))
                 entries))))

(defun hatena-d-list-refresh (&optional type)
  (setq tabulated-list-use-header-line nil
        tabulated-list-format
        (vector
         `(" " 1 t)
         `("Title" ,hatena-d-title-width t)
         `("Date" ,hatena-d-date-width t :right-align t)
         `("Categories" ,hatena-d-category-width t)))
  (let* ((type (or type hatena-d-type 'blog))
         (user (hatena:d:user))
         (entries (hatena:d:api:entries type user)))
    (setq hatena-d-type type
          hatena-d-current-page 1)
    (hatena-d-list-append entries t))
  (tabulated-list-init-header))

(defun hatena-d-list-auto-pager ()
  (when (and hatena:d:auto-pager (eobp))
    (forward-line -1)
    (hatena:d:list-retrieve-next)))

(defun hatena-d-list-put-mark (mark)
  (let* ((point (point))
         (entry (tabulated-list-get-id))
         (cols (hatena-d-list-get-cols))
         (pos (line-beginning-position))
         (inhibit-read-only t))
    (when entry
      (aset cols 0 mark)
      (beginning-of-line)
      (delete-region pos (1+ pos))
      (insert mark)
      (hatena-d-list-add-line-properties
       'tabulated-list-id entry 'tabulated-list-entry cols)
      (goto-char point))))

(defun hatena-d-list-delete ()
  (let ((entry (tabulated-list-get-id))
        (inhibit-read-only t))
    (setq tabulated-list-entries
          (loop for e in tabulated-list-entries
                unless (eq entry (car e))
                collect e))
    (when entry
      (delete-region (line-beginning-position) (1+ (line-end-position))))))

(defun hatena-d-list-apply-face (face)
  (let ((cols (hatena-d-list-get-cols))
        (prop 'font-lock-face)
        (inhibit-read-only t))
    (loop for i below (length cols)
          for col = (propertize (aref cols i) prop face)
          do (aset cols i col))
    (hatena-d-list-add-line-properties prop face)))

(defun hatena-d-list-add-line-properties (&rest props)
  (add-text-properties (line-beginning-position) (line-end-position) props))

(defun hatena-d-set-file-name (file-name)
  (setq hatena-d-file-name file-name)
  (set-visited-file-name file-name))

(defun hatena-d-entry-unique-name (entry)
  (let ((id (hatena:d:entry-id entry))
        (type (hatena:d:entry-type entry)))
    (if id
        (car-safe (last (split-string id ":")))
      (format "new-hatena-%s-entry" type))))

(defun hatena-d-parse-content ()
  (let (pos title)
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward "^\\*" nil t))
          (setq title (read-string "Title: "))
        (setq pos (point))
        (re-search-forward "$" nil t)
        (setq title (buffer-substring-no-properties pos (point))))
      (skip-chars-forward " \t\r\n")
      (list title (buffer-substring-no-properties (point) (point-max))))))

(defun hatena-d-set-revert-function ()
  (set (make-local-variable 'revert-buffer-function) 'hatena:d:revert))

(provide 'hatena-diary)
;;; hatena-diary.el ends here
