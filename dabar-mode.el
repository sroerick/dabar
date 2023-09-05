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
        (book-info nil))
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
                  (push (cons bname (nreverse chapters)) book-info))))))))
    (nreverse book-info)))

;; (get_book_and_chapter_info)



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

(defun get_chapter (target-book target-chapter xml)
  (let ((root (car xml))
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
    (if found
        (let ((buf (get-buffer-create bible-buffer-name))
              (formatted-chapter (format-chapter chapter-contents)))
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
      (message "Chapter not found"))))






;; (get_chapter "Genesis" "1" parsed-xml)
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
  ;; Parse the passage to get the book and chapter
  (if (string-match "\\([A-Za-z ]+\\) \\([0-9]+\\)\\(?::\\([0-9]+\\(?:-[0-9]+\\)?\\)?\\)?" path)
      (let* ((book (match-string 1 path))
             (chapter (match-string 2 path))
             ;; Construct the org-roam file path
             (roam-path (format "%s/Dabar/%s/%s.org" org-roam-directory book chapter)))
        (if (file-exists-p roam-path)
            ;; If the org-roam file exists, open it
            (find-file roam-path)
          ;; If the org-roam file doesn't exist, create and open it
          (find-file roam-path)
          (insert (format "#+title: %s %s\n" book chapter))
          ;; Add the ID to make it an Org-roam node
          (org-id-get-create)
          (insert "\n")))
    ;; If parsing fails, revert to the original behavior
    (browse-url (format "https://www.biblegateway.com/passage/?search=%s" path))))



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




(provide 'dabar-mode)


