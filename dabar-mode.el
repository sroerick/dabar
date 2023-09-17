
(require 'cl-lib)
(load "abbreviations")
;; set variable
(with-temp-buffer
  (insert-file-contents "~/dev/dabar/bible_web.xml")
  (setq xml-string (buffer-string)))

;; parse the buffer
(defvar parsedxml nil)
(setq parsed-xml (with-temp-buffer
		    (insert xml-string)
		    (xml-parse-region (point-min) (point-max))))

(defvar org-roam-directory "/home/roericksweeney/org-roam/")
(defvar dabar-directory "/home/roericksweeney/org-roam/dabar/")

(defvar bible-buffer-name "*Dabar Browse*")


;; minor mode
(defvar dabar-mode-keymap (make-sparse-keymap)
  "Keymap for dabar-mode")


(define-minor-mode dabar-mode
  "read the word of God"
  :lighter " Dabar"
  :keymap dabar-mode-keymap
  (if dabar-mode
      (progn
	(message "Activated dabar-mode"))
    (message "Deactivated dabar-mode")))

(defun activate-dabar-mode-in-directory ()
  "Activate `dabar-mode` if the current file is in a certain directory."
  (let ((dir (file-name-directory buffer-file-name))
        (target-dir (expand-file-name "~/org-roam/")))
    (message "Current directory: %s" dir)
    (message "Target directory: %s" target-dir)
    (when (string-match-p (regexp-quote target-dir) dir)
      (message "Directories matched!")
      (dabar-mode 1))))

;; list of books
(defun get_book_list ()
  "Get a list of books"
(let ((root (car parsed-xml)))
  (let ((children (xml-node-children root))
        (book-names nil))
    (dolist (child children)
      (unless (stringp child)
	(let ((attributes (xml-node-attributes child)))
	  (let ((bname (cdr (assq 'bname attributes))))
	    (when bname
	      (push bname book-names))))))
    (message "Names of all books: %s" (nreverse book-names))))
)
;; (get_book_list)



(defun get_book_and_chapter_info ()
  (let ((root (car parsed-xml))
        (book-info-alist nil))
    (let ((children (xml-node-children root)))
      (dolist (child children)
        (unless (stringp child)
          (let ((attributes (xml-node-attributes child)))
            (let ((bname (cdr (assq 'bname attributes))))
              (when bname
                (let ((chapters nil)
                      (biblebook-children (xml-node-children child)))
                  (dolist (chapter-node biblebook-children)
                    (unless (stringp chapter-node)
                      (let ((chapter-attributes (xml-node-attributes chapter-node)))
                        (let ((cnumber (cdr (assq 'cnumber chapter-attributes))))
                          (when cnumber
                            (push cnumber chapters))))))
                  (push (cons (intern bname) (nreverse chapters)) book-info-alist))))))))
    (nreverse book-info-alist)))

(setq book-and-chapters (get_book_and_chapter_info))


(defun format-chapter (chapter-contents)
  (let ((formatted-text ""))
    (dolist (verse chapter-contents)
      (unless (stringp verse)
	(let* ((attributes (xml-node-attributes verse))
	       (vnumber (cdr (assq 'vnumber attributes)))
	       (verse-text (nth 2 verse)))
	  (setq formatted-text (concat formatted-text (format "%s %s\n" vnumber verse-text))))))
  formatted-text))
    

(defun find-chapter-contents (target-book target-chapter)
  "Return the contents of the specified chapter from the parsed XML."
  (let ((root (car parsed-xml))
        (chapter-contents nil)
        (found nil))
    (dolist (biblebook (xml-get-children root 'BIBLEBOOK) (and found chapter-contents))
      (let ((bname (cdr (assq 'bname (xml-node-attributes biblebook)))))
        (when (and (not found) (string= bname target-book))
          (dolist (chapter (xml-get-children biblebook 'CHAPTER))
            (let ((cnumber (cdr (assq 'cnumber (xml-node-attributes chapter)))))
              (when (and (not found) (string= cnumber target-chapter))
                (setq chapter-contents (xml-node-children chapter))
                (setq found t)))))))))

(defun find-formatted-chapter-contents (target-book target-chapter)
  "Return the formatted contents of the specified chapter from the parsed XML."
  (let ((root (car parsed-xml))
        (chapter-contents nil)
        (found nil))
    (dolist (biblebook (xml-get-children root 'BIBLEBOOK) (and found chapter-contents))
      (let ((bname (cdr (assq 'bname (xml-node-attributes biblebook)))))
        (when (and (not found) (string= bname target-book))
          (dolist (chapter (xml-get-children biblebook 'CHAPTER))
            (let ((cnumber (cdr (assq 'cnumber (xml-node-attributes chapter)))))
              (when (and (not found) (string= cnumber target-chapter))
                (setq chapter-contents (xml-node-children chapter))
                (setq found t)))))))
    (when found
      (setq chapter-contents (format-chapter chapter-contents)))
    chapter-contents))

(defun open-chapter-in-dabar-buffer (target-book target-chapter contents)
  "Open the provided chapter contents in a Dabar buffer."
  (if contents
      (let ((buf (get-buffer-create bible-buffer-name))
            (formatted-chapter (format-chapter contents)))
        (switch-to-buffer buf)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "%s %s: \n\n" target-book target-chapter))
        (insert formatted-chapter)
        (goto-char (point-min))
        (set (make-local-variable 'current-book) target-book)
        (set (make-local-variable 'current-chapter) target-chapter)
        (dabar-mode 1)
        (setq buffer-read-only t))
    (message "Chapter not found")))

(defun get_chapter (target-book target-chapter)
  "This inputs a book and chapter and loads it into a buffer for browsing."
  (let ((contents (find-chapter-contents target-book target-chapter)))
    (open-chapter-in-dabar-buffer target-book target-chapter contents)))



;; (get_chapter "Genesis" "1")
;; cant get this to work
(define-key dabar-mode-keymap (kbd "C-c b c") 'get_chapter)


(defun get-next-chapter ()
  (interactive)
  ;; Assuming `current-book` and `current-chapter` are buffer-local variables in the chapter display buffer
  (let ((next-item (get-next-chapter-item current-chapter book-and-chapters))) ;; Assume book-alist is defined and accessible
    (if next-item
        (progn
          ;; Fetch and display the next chapter
          (get_chapter (car next-item) (cdr next-item) parsed-xml) ;; Assume parsed-xml is defined and accessible
          ;; Update buffer-local variables
          (set (make-local-variable 'current-book) (car next-item))
          (set (make-local-variable 'current-chapter) (cdr next-item)))
      (message "You are at the last chapter"))))

(defun get-next-chapter-item (current-item book-info-alist)
  (let ((found nil)
        (result nil)
        (current-book nil))
    (catch 'done
      (dolist (pair book-info-alist)
        (setq current-book (car pair))
        (dolist (item (cdr pair))
          (if found
              (progn
                (setq result (cons current-book item))
                (throw 'done t)))
          (when (equal current-item item)
            (setq found t)))))
    (if (not found)
        (error "Item not found in list"))
    result))
;; Keybinding to "turn the page"
(define-key dabar-mode-keymap (kbd "C-c n") 'get-next-chapter)


(defun get-previous-chapter ()
  (interactive)
  ;; Assuming `current-book` and `current-chapter` are buffer-local variables in the chapter display buffer
  (let ((prev-item (get-previous-chapter-item current-chapter book-and-chapters))) ;; Assume book-and-chapters is defined and accessible
    (if prev-item
        (progn
          ;; Fetch and display the previous chapter
          (get_chapter (car prev-item) (cdr prev-item) parsed-xml) ;; Assume parsed-xml is defined and accessible
          ;; Update buffer-local variables
          (set (make-local-variable 'current-book) (car prev-item))
          (set (make-local-variable 'current-chapter) (cdr prev-item)))
      (message "You are at the first chapter"))))

(defun get-previous-chapter-item (current-item book-info-alist)
  (let ((found nil)
        (last-item nil)
        (last-book nil))
    (catch 'done
      (dolist (pair book-info-alist)
        (let ((current-book (car pair)))
          (dolist (item (cdr pair))
            (when (equal current-item item)
              (if last-item
                  (progn
                    (setq found t)
                    (throw 'done (cons last-book last-item)))
                (throw 'done nil)))
            (setq last-item item)
            (setq last-book current-book)))))
    (if (not found)
        (error "Item not found in list"))
    (cons last-book last-item)))

;; Keybinding to go to the previous chapter
(define-key dabar-mode-keymap (kbd "C-c p") 'get-previous-chapter)


(defun extract-bible-metadata (filename)
  "Extract Bible metadata from the given FILENAME.
Returns a cons cell (BOOK . CHAPTER) or nil if not found."
  (when (and filename (string-match "/Dabar/\\([^/]+\\)/\\([^/]+\\)\\.org$" filename))
    (cons (match-string 1 filename) (match-string 2 filename))))





;; find verse
(defun get_verse (target-book target-chapter target-verse xml)
(let ((root (car xml))
      (verse-contents nil)
      (found-chapter nil)
      (found-verse nil))
  (dolist (biblebook (xml-get-children root 'BIBLEBOOK) (and found-verse verse-contents))
    (let ((bname (cdr (assq 'bname (xml-node-attributes biblebook)))))
      (when (and (not found-chapter) (string= bname target-book))
	(dolist (chapter (xml-get-children biblebook 'CHAPTER))
	  (let ((cnumber (cdr (assq 'cnumber (xml-node-attributes chapter)))))
	    (when (and (not found-chapter) (string= cnumber target-chapter))
	      (setq found-chapter t)
	      (dolist (verse (xml-get-children chapter 'VERS))
		(let ((vnumber (cdr (assq 'vnumber (xml-node-attributes verse)))))
		  (when (and (not found-verse) (string= vnumber target-verse))
		    (setq verse-contents (xml-node-children verse))
		    (setq found-verse t))))))))))
  (if found-verse
       (message "%s %s:%s. %s" target-book target-chapter target-verse verse-contents)
       (message "Verse not found"))))
;; (get_verse "Genesis" "1" "1" parsed-xml)



(defun org-bible-follow (path)
  "Follow a Bible link."
  (message "Received path: %s" path)
  ;; Parse the passage from the custom URI scheme
  (if (string-match "\\([A-Za-z ]+\\)\\([0-9]+\\)\\(?::\\([0-9]+\\)?\\)?" path)
      (let* ((book (match-string 1 path))
             (chapter (match-string 2 path))
             ;; Construct the reference for org-roam
             (reference (concat "bible:" (replace-regexp-in-string " \\|:.*" "" path)))
             ;; Construct the org-roam file path based on the book and chapter
             (roam-path (format "%s/Dabar/%s/%s.org" org-roam-directory book chapter)))
        (if (file-exists-p roam-path)
            ;; If the org-roam file exists, open it
            (find-file roam-path)
          ;; If the org-roam file doesn't exist, create and open it
          (progn
            (find-file roam-path)
	    (org-id-get-create)
            (org-roam-ref-add reference)

            (insert "\n"))))
      ;; If parsing fails, show a message
      (message "Chapter not found")))


(defun org-bible-export (path desc format)
  "Export a Bible link."
  (cond
   ((eq format 'html) (format "<a href=\"https://www.biblegateway.com/passage/?search=%s\">%s</a>" path (or desc path)))
   ((eq format 'latex) (format "\\href{https://www.biblegateway.com/passage/?search=%s}{%s}" path (or desc path)))
   ((eq format 'ascii) (format "%s" (or desc path)))
   (t path)))


(org-link-set-parameters "bible"
                         :follow #'org-bible-follow
                         :export #'org-bible-export)



(defun insert-bible-text ()
  "Insert the related Bible chapter at the end of the buffer."
  (let ((metadata (extract-bible-metadata (buffer-file-name)))
        (delineator "\n--------------\n"))
    (when metadata
      (let* ((book (car metadata))
            (chapter (cdr metadata))
            (chapter-text (find-formatted-chapter-contents book chapter)))
        (when chapter-text
          ;; Avoid duplicate insertion
          (unless (save-excursion
                    (goto-char (point-max))
                    (let ((found-pos (search-backward delineator nil t)))
                      (message "Delineator found at position: %s" found-pos)
                      found-pos))
            ;; Move to the end of the buffer and insert
            (goto-char (point-max))
            (insert delineator)
            (insert chapter-text)))))))

(defun remove-inserted-bible-text ()
  "Remove the inserted Bible text and the delineator from the end."
  (let ((delineator "\n--------------\n"))
    (goto-char (point-max))
    ;; Check if the buffer ends with the inserted Bible text preceded by the delineator
    (when (search-backward delineator nil t)
      ;; Delete everything from the beginning of the delineator to the end of the buffer
      (delete-region (point) (point-max)))))


(defun load-bible-chapter ()
  "Identify if the current note is inside the 'Dabar' directory and manage the Bible chapter insertion."
  ;; Check if this note is inside the 'Dabar' directory and follows the correct structure
  (when (extract-bible-metadata (buffer-file-name))
    (insert-bible-text)
    
    ;; Add custom functions to the relevant hooks for this buffer
    (add-hook 'before-save-hook 'remove-inserted-bible-text nil t)
    (add-hook 'after-save-hook 'insert-bible-text nil t)
    
    ;; Disable auto-save for the current buffer
    (set (make-local-variable 'auto-save-default) nil)))


;; add a hook so that it checks for Bible metadata every time an org-roam file is opened
(add-hook 'find-file-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (load-bible-chapter))))

(defun find-book-full-form (abbr)
  "Find the full form of a Bible book given its abbreviation."
  (catch 'found
    (dolist (entry bible-alist)
      (when (member abbr entry)
        (throw 'found (car entry))))))



(defun insert-bible-link ()
  "Prompt the user for a Bible verse and insert a formatted link."
  (interactive)
  ;; Prompt the user for the Bible verse.
  (let* ((verse (read-string "Enter Bible verse (e.g., Genesis 1:1): "))
         (book-abbr (car (split-string verse " ")))
         (full-form (find-book-full-form book-abbr)))
    (unless full-form
      (error "Invalid book abbreviation"))
    (let ((link-target (concat "bible:" (replace-regexp-in-string " \\|:.*" "" full-form))))
      ;; Insert the link.
      (insert (format "[[%s][%s]]" link-target verse)))))

(defun get-chapter-prompt)


;; You can bind the function to a key combination if needed.
(global-set-key (kbd "C-c b b") 'insert-bible-link)



(provide 'dabar-mode)


