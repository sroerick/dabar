(require 'cl-lib)

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
    

;; get target chapter
(defun get_chapter (target-book target-chapter xml)
(let ((root (car xml))
      (chapter-contents nil)
      (found nil))
  (dolist (biblebook (xml-get-children root 'BIBLEBOOK) (and found chapter-contents))
    (let ((bname (cdr (assq 'bname (xml-node-attributes biblebook)))))
      (when (and (not found) (string= bname target-book)
	(dolist (chapter (xml-get-children biblebook 'CHAPTER))
	  (let ((cnumber (cdr (assq 'cnumber (xml-node-attributes chapter)))))
	    (when (and (not found) (string= cnumber target-chapter))
	      (setq chapter-contents (xml-node-children chapter))
	      (setq found t))))))))
  (if found
      (let ((buf (generate-new-buffer (format "*%s %s*" target-book target-chapter)))
          (formatted-chapter (format-chapter chapter-contents)))
        (switch-to-buffer buf)
        (insert (format "%s %s: \n\n" target-book target-chapter))
        (insert formatted-chapter)
	(goto-char (point-min))
	(set (make-local-variable 'current-book) target-book)
	(set (make-local-variable 'current-chapter) target-chapter)
	(dabar-mode 1)
        (setq buffer-read-only t))
    (message "Chapter not found"))))



(defvar bible-buffer-name "*Bible Reading*")

;; (defun get_chapter (target-book target-chapter)
;;   "This inputs a book and chapter and loads it into a buffer for browsing. This logic
;; Should ultimately be parsed out into two different functions - one for returning the chapter
;; contents and another for opening in a dabar buffer"
;;   (let ((root (car parsed-xml))
;;         (chapter-contents nil)
;;         (found nil))
;;     (dolist (biblebook (xml-get-children root 'BIBLEBOOK) (and found chapter-contents))
;;       (let ((bname (cdr (assq 'bname (xml-node-attributes biblebook)))))
;;         (when (and (not found) (string= bname target-book))
;;           (dolist (chapter (xml-get-children biblebook 'CHAPTER))
;;             (let ((cnumber (cdr (assq 'cnumber (xml-node-attributes chapter)))))
;;               (when (and (not found) (string= cnumber target-chapter))
;;                 (setq chapter-contents (xml-node-children chapter))
;;                 (setq found t)))))))
;;     (if found
;;         (let ((buf (get-buffer-create bible-buffer-name))
;;               (formatted-chapter (format-chapter chapter-contents)))
;;           (switch-to-buffer buf)
;;           (setq buffer-read-only nil)
;;           (erase-buffer)
;;           (insert (format "%s %s: \n\n" target-book target-chapter))
;;           (insert formatted-chapter)
;;           (goto-char (point-min))
;;           (set (make-local-variable 'current-book) target-book)
;;           (set (make-local-variable 'current-chapter) target-chapter)
;;           (dabar-mode 1)
;;           (setq buffer-read-only t))
;;       (message "Chapter not found"))))


q

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



(defun extract-bible-metadata (filename)
  "Extract Bible metadata from the given FILENAME.
Returns a cons cell (BOOK . CHAPTER) or nil if not found."
  (when (and filename (string-match "/bible/\\([^/]+\\)/\\([^/]+\\)\\.org$" filename))
    (cons (match-string 1 filename) (match-string 2 filename))))

(defun load-bible-chapter ()
  "Identify if the current note is inside the 'bible' directory and insert the related chapter."
  (let ((metadata (extract-bible-metadata (buffer-file-name))))
    (when metadata
      (let ((book (car metadata))
            (chapter (cdr metadata)))
        (let ((chapter-text (get_bible_chapter book chapter)))
          (when chapter-text
            (insert chapter-text)))))))



;; Optionally, you can add a hook so that it checks for Bible metadata every time an org-roam file is opened
;(add-hook 'find-file-hook
;          (lambda ()
;            (when (derived-mode-p 'org-mode)
;              (load-bible-chapter))))





(provide 'dabar-mode)


