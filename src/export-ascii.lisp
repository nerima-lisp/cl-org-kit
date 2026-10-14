;;;; src/export-ascii.lisp
(in-package #:org-kit)

(defun export-ascii-option (options key default)
  (let ((entry (assoc key options)))
    (if entry (cdr entry) default)))

(defun export-ascii-width (text)
  (table-string-width text))

(defun export-ascii-join (strings separator)
  (with-output-to-string (output)
    (loop for string in strings
          for first-p = t then nil
          unless first-p do (write-string separator output)
          do (write-string string output))))

(defun export-ascii-newlines (count)
  (make-string count :initial-element #\Newline))

(defun export-ascii-lines (text)
  (loop with start = 0
        for newline = (position #\Newline text :start start)
        for end = (or newline (length text))
        collect (subseq text start end)
        while newline
        do (setf start (1+ newline))))

(defun export-ascii-wrap-line (line width)
  (let ((words (export-split-words line))
        (lines nil)
        (current ""))
    (dolist (word words)
      (if (zerop (length current))
          (setf current word)
          (if (<= (+ (export-ascii-width current) 1
                     (export-ascii-width word))
                  width)
              (setf current (concatenate 'string current " " word))
              (progn
                (push current lines)
                (setf current word)))))
    (push current lines)
    (nreverse lines)))

(defun export-ascii-wrap-text (text width &optional preserve-breaks)
  (let ((text (substitute #\Space #\Tab text)))
    (if preserve-breaks
        (export-ascii-join
         (mapcan (lambda (line)
                   (export-ascii-wrap-line line width))
                 (export-ascii-lines text))
         (string #\Newline))
        (export-ascii-join
         (export-ascii-wrap-line text width)
         (string #\Newline)))))

(defun export-ascii-indent (text width)
  (if (zerop width)
      text
      (let ((prefix (make-string width :initial-element #\Space)))
        (with-output-to-string (output)
          (loop with start = 0
                for newline = (position #\Newline text :start start)
                for end = (or newline (length text))
                for line = (subseq text start end)
                do (when (plusp (length line))
                     (write-string prefix output))
                   (write-string line output)
                   (when newline (write-char #\Newline output))
                while newline
                do (setf start (1+ newline)))))))

(defun export-ascii-trim-trailing-newlines (text)
  (let ((last-character
          (position-if-not (lambda (character)
                             (char= character #\Newline))
                           text :from-end t)))
    (if last-character
        (subseq text 0 (1+ last-character))
        "")))

(defun export-ascii-normalize-output (text)
  (concatenate 'string (export-ascii-trim-trailing-newlines text)
               (string #\Newline)))

(defun export-ascii-node-value (node key)
  (or (node-property node key)
      (node-text node)
      ""))

(defun export-ascii-inline-views (views preparation)
  (export-ascii-join
   (mapcar (lambda (view)
             (let* ((text (export-ascii-render-view view preparation))
                    (post-blank (node-post-blank (export-view-node view))))
               (if (plusp post-blank)
                   (concatenate 'string text
                                (make-string post-blank :initial-element #\Space))
                   text)))
           views)
   ""))

(defun export-ascii-view-blank-count (view)
  (let* ((node (export-view-node view))
         (type (node-type node)))
    (if (eq type :section)
        (let ((children (export-view-children view)))
          (if children
              (node-post-blank (export-view-node (car (last children))))
              (node-post-blank node)))
        (node-post-blank node))))

(defun export-ascii-view-gap (previous current)
  (let ((previous-type (node-type (export-view-node previous)))
        (current-type (node-type (export-view-node current))))
    (cond
      ((member current-type '(:headline :inlinetask) :test #'eq)
       (export-ascii-newlines 3))
      ((eq previous-type :plain-list)
       (export-ascii-newlines 1))
      (t (make-string (max 2 (1+ (export-ascii-view-blank-count previous)))
                      :initial-element #\Newline)))))

(defun export-ascii-render-views (views preparation)
  (with-output-to-string (output)
    (let ((previous nil)
          (included-heading-level nil))
      (dolist (view views)
        (let* ((node (export-view-node view))
               (type (node-type node))
               (heading-p (member type '(:headline :inlinetask) :test #'eq))
               (level (and heading-p (or (node-property node :level) 1))))
          (when heading-p
            (setf included-heading-level
                  (if (not (eq (export-view-source-document view)
                               (export-preparation-document preparation)))
                      level
                      (and included-heading-level
                           (> level included-heading-level)
                           included-heading-level))))
          (let ((text (export-ascii-render-view view preparation)))
            (when (and included-heading-level
                       (not heading-p)
                       (not (eq type :section)))
              (setf text (export-ascii-indent text 2)))
          (when (plusp (length text))
            (when previous
              (write-string (export-ascii-view-gap previous view) output))
            (write-string text output)
            (setf previous view))))))))

(defun export-ascii-section-margin (node)
  (let ((parent (node-parent node)))
    (if (and parent
             (member (node-type parent) '(:headline :inlinetask) :test #'eq)
             (<= (or (node-property parent :level) 1) 3))
        2
        0)))

(defun export-ascii-current-width (node)
  (let ((parent (node-parent node)))
    (max 1 (- 72
              (cond
                ((and parent (eq (node-type parent) :section))
                 (export-ascii-section-margin parent))
                ((and parent
                      (member (node-type parent) '(:headline :inlinetask)
                              :test #'eq)
                      (<= (or (node-property parent :level) 1) 3))
                 2)
                (t 0))))))

(defun export-ascii-numbering (preparation)
  (let ((counters (make-array 64 :initial-element 0))
        (numbers (make-hash-table :test #'eq)))
    (dolist (view (export-collect-views
                   (export-preparation-root-views preparation)))
      (let* ((node (export-view-node view))
             (type (node-type node)))
        (when (member type '(:headline :inlinetask) :test #'eq)
          (let ((level (max 1 (min 64 (or (node-property node :level) 1)))))
            (incf (aref counters (1- level)))
            (loop for index from level below 64
                  do (setf (aref counters index) 0))
            (setf (gethash view numbers)
                  (loop for index from 0 below level
                        collect (aref counters index)))))))
    numbers))

(defun export-ascii-numbering-enabled-p (options level)
  (let ((value (export-ascii-option options :num t)))
    (and value
         (or (eq value t)
             (and (integerp value) (<= level value))))))

(defun export-ascii-numbering-string (view numbers options &optional toc)
  (let* ((node (export-view-node view))
         (level (or (node-property node :level) 1))
         (numbering (gethash view numbers)))
    (when (and numbering (export-ascii-numbering-enabled-p options level))
      (if toc
          (format nil "~D. " (car (last numbering)))
          (format nil "~{~D~^.~} " numbering)))))

(defun export-ascii-tags (view)
  (let* ((node (export-view-node view))
         (document (export-view-source-document view))
         (tags (effective-heading-tags document node :inherit nil)))
    (when tags
      (format nil ":~{~A~^:~}:" tags))))

(defun export-ascii-heading-first-part (view preparation numbers &optional toc)
  (let* ((node (export-view-node view))
         (options (export-preparation-options preparation))
         (todo (and (export-ascii-option options :todo t)
                    (node-property node :todo-keyword)))
         (priority (and (export-ascii-option options :priority nil)
                        (node-property node :priority)))
         (title (or (node-property node :raw-value) "")))
    (concatenate 'string
                 (or (export-ascii-numbering-string view numbers options toc) "")
                 (if todo (concatenate 'string todo " ") "")
                 (if priority (format nil "(##~A) " priority) "")
                 title)))

(defun export-ascii-heading-title (view preparation numbers text-width
                                    &optional underline toc)
  (let* ((node (export-view-node view))
         (first-part (export-ascii-heading-first-part
                      view preparation numbers toc))
         (tags-enabled (export-ascii-option
                        (export-preparation-options preparation) :tags t))
         (tags (and (not (and toc (eq tags-enabled :not-in-toc)))
                    tags-enabled
                    (export-ascii-tags view)))
         (title (if tags
                    (let* ((tag-width (export-ascii-width tags))
                           (field-width
                             (max (- text-width
                                     (1+ (export-ascii-width first-part)))
                                  tag-width))
                           (space-count (1+ (- field-width tag-width))))
                      (concatenate 'string first-part
                                   (make-string space-count
                                                :initial-element #\Space)
                                   tags))
                    first-part)))
    (if (and underline (not toc) (<= (or (node-property node :level) 1) 3))
        (let ((character (nth (1- (or (node-property node :level) 1))
                              '(#\= #\~ #\-))))
          (format nil "~A~%~A" title
                  (make-string (export-ascii-width first-part)
                               :initial-element character)))
        title)))

(defun export-ascii-build-toc (preparation numbers)
  (let ((entries (export-preparation-toc preparation)))
    (when entries
      (with-output-to-string (output)
        (format output "Table of Contents~%_________________~%~%")
        (loop for entry in entries
              for first-p = t then nil
              do
          (let* ((view (getf entry :view))
                 (level (getf entry :level))
                 (indent (* (1- level) 3))
                 (prefix (if (plusp indent)
                             (concatenate 'string
                                          (make-string (1- indent)
                                                       :initial-element #\.)
                                          " ")
                             ""))
                 (title (export-ascii-heading-title
                         view preparation numbers (- 72 indent) nil t)))
            (unless first-p (write-char #\Newline output))
            (write-string prefix output)
            (write-string title output)))
        (write-string (export-ascii-newlines 3) output)))))

(defun export-ascii-justify-line (line width justification)
  (let* ((difference (max 0 (- width (export-ascii-width line))))
         (padding (case justification
                    (:right difference)
                    (:center (floor difference 2))
                    (otherwise 0))))
    (concatenate 'string (make-string padding :initial-element #\Space) line)))

(defun export-ascii-justify-lines (text width justification)
  (export-ascii-join
   (mapcar (lambda (line)
             (if (plusp (length line))
                 (export-ascii-justify-line line width justification)
                 line))
           (export-ascii-lines text))
   (string #\Newline)))

(defun export-ascii-document-title (preparation)
  (let* ((metadata (export-preparation-metadata preparation))
         (options (export-preparation-options preparation))
         (title (and (export-ascii-option options :title t)
                     (getf metadata :title)))
         (author (and (export-ascii-option options :author t)
                      (getf metadata :author)))
         (date (and (export-ascii-option options :date t)
                    (getf metadata :date)))
         (title (or title ""))
         (author (or author ""))
         (date (or date "")))
    (if (plusp (length title))
        (let* ((title-width
                 (min (max (export-ascii-width title)) 48))
               (formatted-title (export-ascii-wrap-text title title-width))
               (line-width
                 (min 72 (+ 2 (max title-width
                                   (export-ascii-width author)))))
               (line (make-string line-width :initial-element #\_))
               (block
                 (concatenate
                  'string line (export-ascii-newlines 2)
                  (string-upcase formatted-title)
                  (if (plusp (length author))
                      (concatenate 'string (export-ascii-newlines 2) author)
                      "")
                  (export-ascii-newlines 1) line
                  (if (plusp (length date))
                      (concatenate 'string (export-ascii-newlines 3) date)
                      "")
                  (export-ascii-newlines 3))))
          (export-ascii-justify-lines block 72 :center))
        (cond
          ((and (plusp (length author)) (plusp (length date)))
           (concatenate 'string
                        author
                        (make-string
                         (max 1 (- 72 (export-ascii-width author)
                                   (export-ascii-width date)))
                         :initial-element #\Space)
                        date (export-ascii-newlines 3)))
          ((plusp (length date))
           (concatenate 'string
                        (export-ascii-justify-line date 72 :right)
                        (export-ascii-newlines 3)))
          ((plusp (length author))
           (concatenate 'string author (export-ascii-newlines 3)))
          (t "")))))

(defun export-ascii-footnote-number (label preparation)
  (let ((definition
          (find label (export-preparation-footnotes preparation)
                :key (lambda (entry) (getf entry :label))
                :test #'string=)))
    (getf definition :number)))

(defun export-ascii-footnote-definition-text (definition preparation)
  (let* ((view (getf definition :definition))
         (text (export-ascii-render-views
                (export-view-children view) preparation)))
    (export-trim text)))

(defun export-ascii-footnote-section (preparation)
  (let ((footnotes (export-preparation-footnotes preparation)))
    (when footnotes
      (with-output-to-string (output)
        (format output "~%~%~%~%Footnotes~%_________~%~%")
        (loop for definition in footnotes
              for number = (getf definition :number)
              for text = (export-ascii-footnote-definition-text
                          definition preparation)
              for first-line = (format nil "[~D] ~A" number text)
              for wrapped = (export-ascii-wrap-text first-line 72)
              for first-p = t then nil
              do (unless first-p
                   (write-string (export-ascii-newlines 2) output))
                 (write-string wrapped output)
                 )))))

(defun export-ascii-local-heading (link preparation)
  (let* ((type (node-property link :type))
         (path (node-property link :path))
         (target (and path
                      (if (and (plusp (length path))
                               (char= (char path 0) #\#))
                          (subseq path 1)
                          path)))
         (found nil))
    (when (and target
               (member type '("custom-id" "fuzzy") :test #'string=))
      (dolist (view (export-collect-views
                     (export-preparation-root-views preparation)))
        (let ((candidate (export-view-node view)))
          (when (and (member (node-type candidate) '(:headline :inlinetask)
                             :test #'eq)
                     (or (and (string= type "custom-id")
                              (multiple-value-bind (value found-p)
                                  (effective-heading-property-value
                                   candidate "CUSTOM_ID")
                                (and found-p (string= value target))))
                         (and (not (string= type "custom-id"))
                              (string-equal
                               (or (node-property candidate :raw-value) "")
                               target)))
                     (null found))
            (setf found view))))
    found)))

(defun export-ascii-scheme-colon (text)
  (let ((colon (position #\: text)))
    (when (and colon
               (plusp colon)
               (let ((first (char text 0)))
                 (or (<= (char-code #\A) (char-code first) (char-code #\Z))
                     (<= (char-code #\a) (char-code first) (char-code #\z))))
               (loop for index from 1 below colon
                     for character = (char text index)
                     always (or (<= (char-code #\A) (char-code character)
                                    (char-code #\Z))
                                (<= (char-code #\a) (char-code character)
                                    (char-code #\z))
                                (<= (char-code #\0) (char-code character)
                                    (char-code #\9))
                                (find character "+.-"))))
      colon)))

(defun export-ascii-destination-safe-p (raw expanded)
  (and (stringp raw)
       (not (some #'export-control-character-p raw))
       (not (find #\\ raw))
       (stringp expanded)
       (plusp (length expanded))
       (not (some #'export-control-character-p expanded))
       (not (find #\\ expanded))
       (not (find (char expanded 0) '(#\Space #\Tab)))
       (not (and (>= (length expanded) 2)
                 (string= "//" expanded :end1 2 :end2 2)))
       (let ((colon (export-ascii-scheme-colon expanded)))
         (or (null colon)
             (member (string-downcase (subseq expanded 0 colon))
                     '("http" "https" "mailto")
                     :test #'string=)))))

(defun export-ascii-link-source-path (node)
  (let* ((document (node-document node))
         (text (and document (document-text document)))
         (begin (node-begin node))
         (end (and (node-end node)
                   (- (node-end node) (node-post-blank node))))
         (format (node-property node :format)))
    (when (and text begin end)
      (case format
        (:bracket
         (let* ((start (+ begin 2))
                (close (link-object-bracket-close text start end)))
           (and close (subseq text start close))))
        (:angle
         (let* ((start (1+ begin))
                (close (position #\> text :start start :end end)))
           (and close (subseq text start close))))
        (:plain (subseq text begin end))))))

(defun export-ascii-link-path (view preparation)
  (let* ((node (export-view-node view))
         (type (node-property node :type))
         (path (node-property node :path))
         (source-path (or (node-property node :raw-link)
                          (export-ascii-link-source-path node)))
         (raw (if source-path
                  (if (eq (node-property node :format) :bracket)
                      (link-object-normalize-bracket-path
                       (link-object-unescape-path source-path))
                      source-path)
                  (if (and path (stringp type)
                           (member type *org-link-object-types* :test #'string=))
                      (format nil "~A:~A" type path)
                      (or path ""))))
         (expanded (link-object-expand-abbreviation
                    (export-preparation-document preparation) raw)))
    (values raw expanded
            (export-ascii-destination-safe-p (or source-path raw) expanded))))

(defun export-ascii-link-description (view preparation)
  (let ((children (export-view-children view)))
    (and children (export-ascii-inline-views children preparation))))

(defun export-ascii-link-inline (view preparation)
  (let* ((node (export-view-node view))
         (description (export-ascii-link-description view preparation))
         (type (node-property node :type))
         (target-view
           (and (member type '("custom-id" "fuzzy") :test #'string=)
                (export-ascii-local-heading node preparation))))
    (cond
      ((string= (or type "") "radio") (or description ""))
      (target-view
       (let ((target (export-view-node target-view)))
         (if description
             (format nil "[~A]" description)
             (or (node-property target :raw-value) ""))))
      ((string= (or type "") "custom-id")
       (if description
           (format nil "[~A]" description)
           (or (node-property node :path) "")))
      (t
       (multiple-value-bind (raw expanded safe-p)
           (export-ascii-link-path view preparation)
         (declare (ignore raw))
         (cond
           (description (format nil "[~A]" description))
           (safe-p (format nil "<~A>" expanded))
           (t expanded)))))))

(defun export-ascii-link-note (view preparation)
  (let* ((node (export-view-node view))
         (description (export-ascii-link-description view preparation))
         (type (node-property node :type))
         (target-view
           (and (member type '("custom-id" "fuzzy") :test #'string=)
                (export-ascii-local-heading node preparation))))
    (when (and description (plusp (length description)))
      (cond
        ((string= (or type "") "radio") nil)
        (target-view
         (let* ((target (export-view-node target-view))
                (numbers (export-ascii-numbering preparation))
                (numbering (gethash target-view numbers))
                (label
                  (if (and numbering
                           (export-ascii-numbering-enabled-p
                            (export-preparation-options preparation)
                            (or (node-property target :level) 1)))
                      (format nil "~{~D~^.~}" numbering)
                      (or (node-property target :raw-value) ""))))
           (format nil "[~A] See section ~A" description label)))
        ((string= (or type "") "custom-id") nil)
        (t
         (multiple-value-bind (raw expanded safe-p)
             (export-ascii-link-path view preparation)
           (declare (ignore raw))
           (when safe-p
             (export-ascii-wrap-text
              (format nil "[~A] <~A>" description expanded) 72))))))))

(defun export-ascii-view-links (view)
  (labels ((collect (current)
             (if (member (node-type (export-view-node current))
                         '(:headline :inlinetask) :test #'eq)
                 nil
                 (cons current
                       (mapcan #'collect (export-view-children current))))))
    (collect view)))

(defun export-ascii-item-ordered-p (node)
  (let* ((parent (node-parent node))
         (list-type (and parent (node-property parent :type)))
         (bullet (node-property node :bullet)))
    (or (eq list-type :ordered)
        (and (stringp bullet)
             (plusp (length bullet))
             (digit-char-p (char bullet 0))))))

(defun export-ascii-prefix-item-lines (prefix text)
  (let ((indent (make-string (export-ascii-width prefix)
                             :initial-element #\Space)))
    (export-ascii-join
     (loop for line in (export-ascii-lines text)
           for first-p = t then nil
           collect (concatenate 'string (if first-p prefix indent) line))
     (string #\Newline))))

(defun export-ascii-link-notes (view preparation)
  (let ((notes nil))
    (dolist (candidate (export-ascii-view-links view))
      (when (eq (node-type (export-view-node candidate)) :link)
        (let ((note (export-ascii-link-note candidate preparation)))
          (when (and note (not (member note notes :test #'string=)))
            (push note notes)))))
    (nreverse notes)))

(defun export-ascii-render-section-with-notes (view preparation)
  (let* ((section-text
           (export-ascii-indent
            (export-ascii-render-views (export-view-children view) preparation)
            (export-ascii-section-margin (export-view-node view))))
         (notes (export-ascii-link-notes view preparation)))
    (if notes
        (concatenate 'string section-text (export-ascii-newlines 3)
                     (export-ascii-join notes (export-ascii-newlines 2)))
        section-text)))

(defun export-ascii-render-heading-children (view preparation)
  (let* ((children (export-view-children view))
         (first (first children)))
    (if (and first (eq (node-type (export-view-node first)) :section))
        (let* ((first-text (export-ascii-render-view first preparation))
               (rest-text (export-ascii-render-views (rest children)
                                                     preparation)))
          (if (plusp (length rest-text))
              (concatenate 'string first-text
                           (export-ascii-view-gap first (second children))
                           rest-text)
              first-text))
        (export-ascii-render-views children preparation))))

(defun export-ascii-render-headline (view preparation)
  (let* ((node (export-view-node view))
         (numbers (export-ascii-numbering preparation))
         (level (or (node-property node :level) 1))
         (width (max 1 (- 72 (if (> level 3) (* 2 (- level 3)) 0))))
         (title (export-ascii-heading-title view preparation numbers width t))
         (body (export-ascii-render-heading-children view preparation))
         (prefix (if (> level 3)
                     (format nil "~C " (nth (mod (- level 4) 3)
                                              '(#\* #\+ #\-)))
                     "")))
    (if (plusp (length body))
        (concatenate 'string prefix title (export-ascii-newlines 2)
                     (if (plusp (length prefix))
                         (export-ascii-indent body (length prefix))
                         body))
        (concatenate 'string prefix title))))

(defun export-ascii-render-item (view preparation ordinal)
  (let* ((node (export-view-node view))
         (parent (node-parent node))
         (list-type (and parent (node-property parent :type)))
         (checkbox (case (node-property node :checkbox)
                     (:on "[X] ")
                     (:off "[ ] ")
                     (:trans "[-] ")
                     (otherwise "")))
         (bullet (or (node-property node :bullet) "- "))
         (tag-range (node-property node :reader-tag-range))
         (tag (and tag-range
                   (subseq (document-text (node-document node))
                           (car tag-range) (cdr tag-range))))
         (ordered-p (export-ascii-item-ordered-p node))
         (marker
           (cond
             (ordered-p
              (let* ((counter (node-property node :counter))
                     (number (or counter ordinal))
                     (punctuation (or (find-if (lambda (character)
                                                (find character ".)"))
                                              bullet)
                                      #\.)))
                (format nil "~D~C " number punctuation)))
             ((eq list-type :descriptive)
              (if tag (format nil "~A :: " tag) ":: "))
             (t bullet)))
         (prefix (concatenate 'string marker checkbox))
         (children (export-view-children view))
         (content (export-ascii-render-views children preparation))
         (first-child (first children))
         (descriptive-p (eq list-type :descriptive)))
    (cond
      ((not (plusp (length content))) prefix)
      ((and (not descriptive-p) first-child
            (eq (node-type (export-view-node first-child)) :paragraph))
       (let ((content (export-ascii-trim-trailing-newlines content)))
         (export-ascii-prefix-item-lines prefix content)))
      (t
       (concatenate 'string prefix (export-ascii-newlines 1)
                    (export-ascii-indent content
                                         (if descriptive-p 6
                                             (export-ascii-width prefix))))))))

(defun export-ascii-render-list (view preparation)
  (let ((items (export-view-children view)))
    (export-ascii-indent
     (with-output-to-string (output)
       (loop with ordinal = 0
             for item in items
             for first-p = t then nil
             for node = (export-view-node item)
             for ordered-p = (export-ascii-item-ordered-p node)
             do (if ordered-p
                    (incf ordinal)
                    (setf ordinal 0))
                (unless first-p (write-char #\Newline output))
                (write-string (export-ascii-render-item item preparation ordinal)
                              output)))
     2)))

(defun export-ascii-code-box (value)
  (let* ((value (export-ascii-trim-trailing-newlines value))
         (lines (export-ascii-lines value)))
    (if (zerop (length value))
        ""
        (concatenate 'string ",----" (export-ascii-newlines 1)
                     (export-ascii-join
                      (mapcar (lambda (line)
                                (concatenate 'string "| " line))
                              lines)
                      (export-ascii-newlines 1))
                     (export-ascii-newlines 1) "`----"))))

(defun export-ascii-fixed-width-value (value)
  (export-ascii-join
   (mapcar (lambda (line)
             (if (and (plusp (length line)) (char= (char line 0) #\:))
                 (string-left-trim '(#\Space #\Tab) (subseq line 1))
                 line))
           (export-ascii-lines value))
   (string #\Newline)))

(defun export-ascii-table-hline (widths)
  (with-output-to-string (output)
    (write-char #\+ output)
    (loop for width across widths
          do (write-string (make-string (+ width 2) :initial-element #\-) output)
             (write-char #\+ output))))

(defun export-ascii-table-row (row widths alignments preparation)
  (if (eq (node-property (export-view-node row) :type) :rule)
      (export-ascii-table-hline widths)
      (let ((cells (export-view-children row)))
        (with-output-to-string (output)
          (write-char #\| output)
          (loop for cell in cells
                for column from 0
                for node = (export-view-node cell)
                for value = (if (export-view-children cell)
                                (export-ascii-inline-views
                                 (export-view-children cell) preparation)
                                (or (node-property node :value) ""))
                for width = (aref widths column)
                for alignment = (aref alignments column)
                do (write-string (table-format-cell value width alignment)
                                 output)
                   (write-char #\| output))))))

(defun export-ascii-render-table (view preparation)
  (let* ((node (export-view-node view))
         (rows (export-view-children view))
         (table-type (node-property node :type)))
    (if (eq table-type :table.el)
        (export-ascii-trim-trailing-newlines
         (or (node-property node :value) ""))
        (let* ((row-views
                 (remove-if
                  (lambda (row)
                    (eq (node-property (export-view-node row) :type) :rule))
                  rows))
               (column-count (loop for row in row-views
                                   maximize (length (export-view-children row))
                                     into maximum
                                   finally (return (or maximum 0))))
               (maximum-width
                 (limit-value
                  (document-limits
                   (export-preparation-document preparation))
                  :max-output-length)))
          (multiple-value-bind (widths alignments)
              (table-column-layout
               row-views column-count maximum-width
               (lambda (row)
                 (mapcar
                  (lambda (cell)
                    (if (export-view-children cell)
                        (export-ascii-inline-views
                         (export-view-children cell) preparation)
                        (or (node-property (export-view-node cell) :value) "")))
                  (export-view-children row))))
            (export-ascii-join
             (mapcar (lambda (row)
                       (export-ascii-table-row row widths alignments preparation))
                     rows)
             (string #\Newline)))))))

(defun export-ascii-render-source-block (view preparation)
  (let* ((node (export-view-node view))
         (source (export-ascii-code-box (or (node-property node :value) "")))
         (results (export-ascii-render-views
                   (export-view-children view) preparation)))
    (export-ascii-join
     (remove-if (lambda (text)
                  (zerop (length text)))
                (list source results))
     (export-ascii-newlines 2))))

(defun export-ascii-render-view (view preparation)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view))
         (child-text (lambda ()
                       (export-ascii-render-views children preparation))))
    (case type
      ((:org-data :special-block :drawer :center-block) (funcall child-text))
      ((:headline :inlinetask) (export-ascii-render-headline view preparation))
      (:section
       (export-ascii-render-section-with-notes view preparation))
      (:paragraph
       (export-ascii-wrap-text
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (export-ascii-inline-views children preparation))
        (export-ascii-current-width node)
        (cdr (assoc :preserve-breaks
                    (export-preparation-options preparation)))))
      (:plain-list (export-ascii-render-list view preparation))
      (:table (export-ascii-render-table view preparation))
      ((:babel-call :inline-babel-call :citation :citation-reference)
       (or (node-text node) ""))
      ((:example-block :src-block)
       (if (eq type :src-block)
           (export-ascii-render-source-block view preparation)
           (export-ascii-code-box (export-ascii-node-value node :value))))
      (:fixed-width
       (export-ascii-code-box
        (export-ascii-fixed-width-value (export-ascii-node-value node :value))))
      (:dynamic-block (export-ascii-node-value node :value))
      (:export-block
       (if (string-equal (or (node-property node :type) "") "ASCII")
           (export-ascii-node-value node :value)
           ""))
      (:export-snippet
       (if (string-equal (or (node-property node :back-end) "") "ascii")
           (or (node-property node :value) "")
           ""))
      (:horizontal-rule (make-string 72 :initial-element #\-))
      (:footnote-definition "")
      ((:bold :italic)
       (let ((delimiter (if (eq type :bold) "*" "/")))
         (format nil "~A~A~A" delimiter (funcall child-text) delimiter)))
      ((:strike-through :underline :subscript :superscript) (funcall child-text))
      ((:code :verbatim)
       (format nil "`~A'" (or (node-property node :value) "")))
      (:entity (or (node-property node :utf-8)
                   (node-property node :utf8)
                   (node-property node :ascii)
                   (node-text node)
                   ""))
      (:footnote-reference
       (let ((label (node-property node :label)))
         (if label
             (let ((number (export-ascii-footnote-number label preparation)))
               (format nil "[~A]" number))
             (funcall child-text))))
      (:inline-src-block (or (node-property node :value) ""))
      (:line-break (string #\Newline))
      (:link (export-ascii-link-inline view preparation))
      (:macro (funcall child-text))
      (:plain-text
       (substitute #\Space #\Tab (or (node-text node) "")))
      (:radio-target "")
      (:target "")
      (:statistics-cookie (or (node-text node) ""))
      (:timestamp (or (node-text node) ""))
      (:quote-block (export-ascii-indent (funcall child-text) 6))
      (:verse-block (export-ascii-indent (funcall child-text) 6))
      ((:latex-environment :latex-fragment) (export-ascii-node-value node :value))
      (otherwise (funcall child-text)))))

(defun export-ascii-visible-control-character-p (character)
  (let ((code (char-code character)))
    (and (not (char= character #\Newline))
         (or (<= 0 code #x1f)
             (<= #x7f code #x9f)))))

(defun export-ascii-sanitize-controls (text)
  (if (notany #'export-ascii-visible-control-character-p text)
      text
      (with-output-to-string (output)
        (loop for character across text
              for code = (char-code character)
              do (if (export-ascii-visible-control-character-p character)
                     (if (<= #x80 code #x9f)
                         (format output "\\u~4,'0X" code)
                         (format output "\\x~2,'0X" code))
                     (write-char character output))))))

(defun export-ascii-render-preparation (preparation)
  (let* ((numbers (export-ascii-numbering preparation))
         (title (export-ascii-document-title preparation))
         (toc (export-ascii-build-toc preparation numbers))
         (body (export-ascii-render-views
                (export-preparation-root-views preparation) preparation))
         (footnotes (export-ascii-footnote-section preparation))
         (text (concatenate 'string title (or toc "") body (or footnotes "")))
         (text (export-ascii-normalize-output text))
         (text (export-ascii-sanitize-controls text)))
    (check-limit (document-limits (export-preparation-document preparation))
                 :max-output-length (length text))
    text))

(defmethod export-render-preparation
    ((preparation export-preparation) (backend (eql :ascii)))
  (declare (ignore backend))
  (export-ascii-render-preparation preparation))
