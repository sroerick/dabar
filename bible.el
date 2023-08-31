(require 'cl-lib)

;; set variable
(with-temp-buffer
  (insert-file-contents "~/bible_web.xml")
  (setq xml-string (buffer-string)))

;; parse the buffer
(defvar parsedxml nil)
(setq parsed-xml (with-temp-buffer
		    (insert xml-string)
		    (xml-parse-region (point-min) (point-max))))

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
(get_book_list)


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
      (target-book "Revelation")
      (target-chapter "1")
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
        (setq buffer-read-only t))
    (message "Chapter not found")))
)

(get_chapter "Genesis" "1" parsed-xml)
;; cant get this to work
(define-key dabar-mode-keymap (kbd "C-c b c") 'get_chapter)


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
       (message "Verse not found")))
)
(get_verse "Genesis" "1" "1" parsed-xml)
