;;;; src/export-org.lisp
(in-package #:org-kit)

(defstruct (export-org-backend-state (:constructor export-org-backend-make-state))
  (buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (base-offset 0)
  limits
  (emitted-footnotes (make-hash-table :test #'equal)))

(defun export-org-backend-state (context)
  (export-render-context-state context))

(defun export-org-backend-append (context text)
  (unless (stringp text)
    (error 'type-error :datum text :expected-type 'string))
  (unless (zerop (length text))
    (let* ((state (export-org-backend-state context))
           (buffer (export-org-backend-state-buffer state))
           (start (fill-pointer buffer))
           (end (+ start (length text)))
           (actual (+ (export-org-backend-state-base-offset state) end))
           (limit (limit-value (export-render-context-limits context)
                               :max-output-length)))
      (check-limit (export-render-context-limits context)
                   :max-output-length actual)
      (when (> end (array-total-size buffer))
        (let ((capacity
                (min limit
                     (max end (if (zerop (array-total-size buffer))
                                  end
                                  (* 2 (array-total-size buffer)))))))
          (setf buffer (adjust-array buffer capacity :fill-pointer start)
                (export-org-backend-state-buffer state) buffer)))
      (setf (fill-pointer buffer) end)
      (replace buffer text :start1 start)))
  text)

(defun export-org-backend-capture (context function)
  (let* ((state (export-org-backend-state context))
         (outer-buffer (export-org-backend-state-buffer state))
         (outer-base (export-org-backend-state-base-offset state))
         (inner-buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (inner-base (+ outer-base (fill-pointer outer-buffer)))
         (result nil))
    (setf (export-org-backend-state-buffer state) inner-buffer
          (export-org-backend-state-base-offset state) inner-base)
    (unwind-protect
        (progn
          (funcall function)
          (setf result (coerce (export-org-backend-state-buffer state) 'string)))
      (setf (export-org-backend-state-buffer state) outer-buffer
            (export-org-backend-state-base-offset state) outer-base))
    result))

(defun export-org-backend-normalize-element (text)
  (if (zerop (length text))
      text
      (let ((position (length text))
            (trimmed-p nil))
        (loop (let ((before-spaces position))
                (loop while (and (plusp position) (find (char text (1- position)) '(#\Space #\Tab)))
                      do (decf position))
                (if (and (plusp position) (char= (char text (1- position)) #\Newline))
                    (setf position (1- position)
                          trimmed-p t)
                    (progn
                      (unless trimmed-p
                        (setf position before-spaces))
                      (return)))))
        (concatenate 'string (subseq text 0 position) (string #\Newline)))))

(defun export-org-backend-render-element (context function)
  (export-org-backend-normalize-element (export-org-backend-capture context function)))

(defun export-org-backend-render-object (context function)
  (export-org-backend-capture context function))

(defun export-org-backend-render-child (context view)
  (let* ((node (%export-view-node view))
         (parent (node-parent node))
         (preamble-keyword-p
           (and (eq (node-type node) :keyword)
                (stringp (node-property node :key))
                (member (node-property node :key)
                        '("TITLE" "AUTHOR" "CREATOR" "DATE" "EMAIL"
                          "OPTIONS" "PROPERTY")
                        :test #'string=)
                parent
                (eq (node-type parent) :section)
                (node-parent parent)
                (eq (node-type (node-parent parent)) :org-data)))
         (rendered
           (let ((output (export-render-view context view)))
             (if preamble-keyword-p
                 (export-node-strip-post-blank node output)
                 output)))
         (suppressed-p
           (case (node-type node)
             (:planning (not (export-org-backend-option-enabled-p
                              context :planning nil)))
             (:clock (not (export-org-backend-option-enabled-p
                           context :clock nil)))
             (:property-drawer
              (not (export-org-backend-option-enabled-p
                    context :properties nil)))
             (:drawer
              (let* ((policy (export-option-value context :drawers))
                     (name (node-property node :drawer-name)))
                (not (or (eq policy t)
                         (and (consp policy)
                              (export-option-selects-name-p policy name))))))
             (:export-block
              (not (string-equal (or (node-property node :type) "") "ORG")))
             (:footnote-definition t)
             (:src-block
              (or (eq (getf (%export-view-payload view) :exports) :none)
                  (string= rendered (export-node-post-blank-string node)))))))
    (unless (and suppressed-p
                 (string= rendered (export-node-post-blank-string node)))
      (export-org-backend-append context rendered))))

(defun export-org-backend-render-children (context views &optional source-document)
  (let ((previous-document nil))
    (dolist (view views)
      (let* ((view-document (export-org-backend-source-document view))
             (rendered
               (export-org-backend-capture
                context
                (lambda ()
                  (export-org-backend-render-child context view)))))
        (when (and source-document previous-document view-document
                   (not (eq previous-document view-document)))
          (let* ((state (export-org-backend-state context))
                 (buffer (export-org-backend-state-buffer state))
                 (end (fill-pointer buffer)))
            (cond
              ((eq previous-document source-document)
               (loop while (and (> end 1)
                                (char= (char buffer (1- end)) #\Newline)
                                (char= (char buffer (- end 2)) #\Newline))
                     do (decf end)
                        (setf (fill-pointer buffer) end)))
              ((eq view-document source-document)
               (let ((trailing-newlines
                       (loop for index downfrom (1- end) to 0
                             while (char= (char buffer index) #\Newline)
                             count t)))
                 (when (< trailing-newlines 2)
                   (export-org-backend-append
                    context (make-string (- 2 trailing-newlines)
                                         :initial-element #\Newline))))))))
        (export-org-backend-append context rendered)
        (setf previous-document view-document)))))

(defun export-org-backend-view-node (view)
  (%export-view-node view))

(defun export-org-backend-view-children (view)
  (%export-view-children view))

(defun export-org-backend-source-document (view)
  (or (%export-view-source-document view) (node-document (%export-view-node view))))

(defun export-org-backend-source-end (node)
  (let ((end (node-end node)))
    (when end
      (max (or (node-begin node) 0) (- end (node-post-blank node))))))

(defun export-org-backend-attribute-line-p (text start end newline-p)
  (when newline-p
    (let ((position start))
      (loop while (and (< position end) (find (char text position) '(#\Space #\Tab)))
            do (incf position))
      (when
          (and (<= (+ position 7) end)
               (char= (char text position) #\#)
               (char= (char text (1+ position)) #\+)
               (string-equal text "ATTR_" :start1 (+ position 2) :end1 (+ position 7)))
        (incf position 7)
        (let ((name-start position))
          (loop while (and (< position end)
                           (let ((code (char-code (char text position))))
                             (or (<= (char-code #\A) code (char-code #\Z))
                                 (<= (char-code #\a) code (char-code #\z))
                                 (<= (char-code #\0) code (char-code #\9))
                                 (find (char text position) "-_"))))
                do (incf position))
          (and (> position name-start)
               (< position end)
               (char= (char text position) #\:)
               (or (= (1+ position) end) (char= (char text (1+ position)) #\Space))))))))

(defun export-org-backend-append-source-range (context text start end)
  (let ((position start))
    (loop while (< position end)
          for newline = (position #\Newline text :start position :end end)
          for line-end = (or newline end)
          for line-after = (if newline (1+ newline) end)
          do (unless
                 (export-org-backend-attribute-line-p
                  text position line-end (not (null newline)))
               (let* ((keyword-start
                        (loop for index from position below line-end
                              while (find (char text index) '(#\Space #\Tab))
                              finally (return index)))
                      (colon
                        (and (< (+ keyword-start 2) line-end)
                             (char= (char text keyword-start) #\#)
                             (char= (char text (1+ keyword-start)) #\+)
                             (position #\: text :start (+ keyword-start 2)
                                                :end line-end))))
                 (if colon
                     (progn
                       (export-org-backend-append context "#+")
                       (export-org-backend-append
                        context
                        (string-downcase
                         (subseq text (+ keyword-start 2) colon)))
                       (if (and (= (1+ colon) line-end)
                                (string-equal
                                 (subseq text (+ keyword-start 2) colon)
                                 "RESULTS"))
                           (progn
                             (export-org-backend-append context ": ")
                             (when newline
                               (export-org-backend-append
                                context (string #\Newline))))
                           (export-org-backend-append
                            context (subseq text colon line-after))))
                     (export-org-backend-append
                      context (subseq text position line-after)))))
             (setf position line-after))))

(defun export-org-backend-render-source-contents (node view context)
  (let* ((document (export-org-backend-source-document view))
         (source (and document (document-text document)))
         (start (node-begin node))
         (end (and source (export-org-backend-source-end node)))
         (remaining (copy-list (%export-view-children view)))
         (cursor start))
    (labels ((code-lines (value)
               (loop with position = 0
                     while (< position (length value))
                     for newline = (position #\Newline value :start position)
                     for line-end = (or newline (length value))
                     collect (list position line-end (not (null newline)))
                     do (setf position (if newline (1+ newline) line-end))))
             (render-code-value (value preserve-indent-p)
               (let* ((lines (code-lines value))
                      (indent
                        (unless preserve-indent-p
                          (loop for (line-start line-end) in lines
                                for content-start =
                                  (loop for index from line-start below line-end
                                        while (find (char value index)
                                                    '(#\Space #\Tab))
                                        finally (return index))
                                when (< content-start line-end)
                                  minimize (- content-start line-start))))
                      (trim (or indent 0)))
                 (with-output-to-string (output)
                   (dolist (line lines)
                     (destructuring-bind (line-start line-end newline-p) line
                       (let ((content-start (+ line-start trim)))
                         (when (and (not preserve-indent-p)
                                    (< content-start line-end))
                           (write-string "  " output))
                         (write-string value output
                                       :start content-start
                                       :end line-end)
                         (when newline-p
                           (write-char #\Newline output)))))
                   (when (and (plusp (length value))
                              (not (char= (char value (1- (length value)))
                                          #\Newline)))
                     (write-char #\Newline output)))))
             (render-code-source ()
               (let* ((name (if (eq (node-type node) :src-block)
                                "src"
                                "example"))
                      (open-marker (concatenate 'string "#+begin_" name))
                      (close-marker (concatenate 'string "#+end_" name))
                      (block-start (or (node-post-affiliated node) start))
                      (opening-end
                        (and start end
                             (position #\Newline source
                                       :start block-start
                                       :end end)))
                      (body-start (and opening-end (1+ opening-end)))
                      (closing-start
                        (and body-start
                             (search close-marker source
                                     :start2 body-start
                                     :end2 end
                                     :from-end t
                                     :test #'char-equal))))
                 (if (and source start end opening-end body-start closing-start)
                     (let* ((header-tail
                              (subseq source
                                      (+ block-start (length open-marker))
                                      opening-end))
                            (value (or (node-property node :value) ""))
                            (preserve-indent-p
                              (eq (node-property node :preserve-indent) t)))
                       (export-org-backend-render-element
                        context
                        (lambda ()
                          (export-org-backend-append-source-range
                           context source start block-start)
                          (export-org-backend-append
                           context
                           (concatenate 'string open-marker header-tail
                                        (string #\Newline)))
                          (export-org-backend-append
                           context
                           (render-code-value value preserve-indent-p))
                          (export-org-backend-append context close-marker))))
                     (export-org-backend-capture
                      context #'render-identity-content))))
             (render-result-content ()
               (export-org-backend-capture
                context
                (lambda ()
                  (export-org-backend-render-children
                   context (%export-view-children view)))))
             (render-identity-content ()
               (if (and source start end (<= start end (length source)))
                   (progn
                     (dolist (child (node-children node))
                       (let* ((child-start (node-begin child))
                              (child-end
                                (if (node-end child)
                                    (min end (node-end child))
                                    (export-org-backend-source-end child))))
                         (when (and child-start child-end
                                    (<= start child-start child-end end)
                                    (>= child-start cursor))
                           (export-org-backend-append-source-range
                            context source cursor child-start)
                           (let ((child-view
                                   (find child remaining
                                         :key #'%export-view-node
                                         :test #'eq)))
                             (when child-view
                               (setf remaining
                                     (delete child-view remaining :test #'eq))
                               (export-org-backend-render-child
                                context child-view)))
                           (setf cursor child-end))))
                     (export-org-backend-append-source-range
                      context source cursor end)
                     (export-org-backend-render-children context remaining))
                   (export-org-backend-render-children context remaining)))
             (render-code-block ()
               (let* ((source-block-p (eq (node-type node) :src-block))
                      (mode
                        (if source-block-p
                            (or (getf (%export-view-payload view) :exports)
                                :code)
                            :code)))
                 (case mode
                   (:none "")
                   (:results
                    (if source-block-p
                        (render-result-content)
                        ""))
                   (:code
                    (render-code-source))
                   (:both
                    (let ((code (render-code-source))
                          (results
                            (if source-block-p
                                (render-result-content)
                                "")))
                      (if (plusp (length results))
                          (concatenate 'string code (string #\Newline)
                                       results)
                          code)))
                   (otherwise
                    (render-code-source))))))
      (if (and (member (node-type node) '(:src-block :example-block) :test #'eq)
               source start end)
          (render-code-block)
          (render-identity-content)))))

(defun export-org-backend-render-identity (node view context)
  (let* ((payload (%export-view-payload view))
         (expanded-p
           (and (consp payload)
                (loop for (key value) on payload by #'cddr
                      thereis (and (eq key :expanded-text) (stringp value)))))
         (rendered
           (cond
             ((eq (node-type node) :table)
              (export-org-backend-render-table node view context))
             (expanded-p
              (export-org-backend-render-object
               context
               (lambda ()
                 (export-org-backend-render-children
                  context (%export-view-children view)))))
             ((or (eq (node-type-category (node-type node)) :object)
                  (eq (node-type node) :src-block)
                  (eq (node-type node) :example-block))
              (export-org-backend-render-object
               context
               (lambda ()
                 (let ((rendered
                         (export-org-backend-render-source-contents node view context)))
                   (when rendered
                     (export-org-backend-append context rendered))))))
             (t
              (export-org-backend-render-element
               context
               (lambda ()
                 (export-org-backend-render-source-contents
                  node view context)))))))
    (export-org-backend-normalize-identity-text
     node
     (if (and expanded-p (plusp (length rendered))
              (char= (char rendered (1- (length rendered))) #\Newline))
         (subseq rendered 0 (1- (length rendered)))
         rendered))))
(defun export-org-backend-render-table (node view context)
  (let* ((rows (node-children node))
         (column-count
           (loop for row in rows
                 maximize (length (node-children row)) into count
                 finally (return (or count 0)))))
    (if (zerop column-count)
        (export-org-backend-render-source-contents node view context)
        (multiple-value-bind (widths alignments)
            (table-column-layout
             rows column-count 100000
             (lambda (row)
               (unless (eq (node-property row :type) :rule)
                 (mapcar (lambda (cell)
                           (node-property cell :value ""))
                         (node-children row)))))
          (with-output-to-string (output)
            (dolist (row rows)
              (if (eq (node-property row :type) :rule)
                  (progn
                    (write-char #\| output)
                    (loop for column below column-count
                          do (when (plusp column)
                               (write-char #\+ output))
                             (write-string (table-hline-segment
                                            (aref widths column))
                                           output))
                    (write-char #\| output))
                  (progn
                    (write-char #\| output)
                    (loop for cell in (node-children row)
                          for column from 0
                          do (write-string
                              (table-format-cell
                               (node-property cell :value "")
                               (aref widths column)
                               (aref alignments column))
                              output)
                             (write-char #\| output))))
              (write-char #\Newline output)))))))
(defun export-org-backend-normalize-identity-text (node text)
  (let* ((parent (node-parent node))
         (bullet (node-property node :bullet))
         (item-start
           (loop for index from 0 below (length text)
                 while (find (char text index) '(#\Space #\Tab))
                 finally (return index)))
         (text
           (if (and (eq (node-type node) :item)
                    parent
                    (eq (node-type parent) :plain-list)
                    (stringp bullet)
                    (not (string= bullet "- "))
                    (<= (+ item-start (length bullet)) (length text))
                    (string= text bullet :start1 item-start
                             :end1 (+ item-start (length bullet))))
               (concatenate 'string (subseq text 0 item-start) "- "
                            (subseq text (+ item-start (length bullet))))
               text))
         (results-p
           (and (eq (node-type node) :fixed-width)
                (let* ((document (node-document node))
                       (source (and document (document-text document)))
                       (start (node-begin node))
                       (marker "#+RESULTS:"))
                  (and source start
                       (<= (+ start (length marker)) (length source))
                       (string-equal source marker
                                     :start1 start
                                     :end1 (+ start (length marker)))))))
         (text
           (if results-p
               (export-node-strip-post-blank node text)
               text))
         (end
           (loop with position = (length text)
                 while (and (plusp position)
                            (char= (char text (1- position)) #\Newline))
                 do (decf position)
                 finally (return position)))
         (line-start
           (1+ (or (position #\Newline text :end end :from-end t) -1)))
         (marker-start (- end 3))
         (star-end (1- marker-start)))
    (if (and (eq (node-type node) :inlinetask)
             (>= marker-start line-start)
             (string= text "END" :start1 marker-start :end1 end)
             (>= star-end line-start)
             (char= (char text star-end) #\Space)
             (plusp
              (loop for index downfrom (1- star-end) to line-start
                    while (char= (char text index) #\*)
                    count t)))
        (concatenate 'string (subseq text 0 marker-start) "end"
                     (subseq text end))
        text)))

(defun export-org-backend-keyword-value (records key)
  (let ((record (find key records :key #'second :test #'string=)))
    (and record (third record))))

(defun export-org-backend-option-enabled-p (context key default)
  (let ((entry (assoc key (export-render-context-options context) :test #'eq)))
    (if entry
        (not (null (cdr entry)))
        default)))

(defun export-org-backend-nonblank-p (value)
  (and (stringp value)
       (some
        (lambda (character)
          (not (export-whitespace-character-p character)))
        value)))

(defun export-org-backend-render-template-header (context)
  (let* ((document (export-render-context-document context))
         (metadata (export-render-context-metadata context))
         (records (export-keyword-records document))
         (options-records
           (labels ((collect-options (view)
                      (let ((node (%export-view-node view)))
                        (append
                         (when (and (eq (export-org-backend-source-document view) document)
                                    (eq (node-type node) :keyword)
                                    (string= (node-property node :key) "OPTIONS"))
                           (list (list node "OPTIONS"
                                       (node-property node :value ""))))
                         (loop for child in (%export-view-children view)
                               append (collect-options child))))))
             (loop for view in (export-render-context-root-views context)
                   append (collect-options view))))
         (options-lines
           (loop for record in options-records
                 collect (format nil "#+options: ~A" (third record))))
         (title (getf metadata :title))
         (date (getf metadata :date))
         (author (getf metadata :author))
         (email (export-org-backend-keyword-value records "EMAIL"))
         (creator (export-org-backend-keyword-value records "CREATOR")))
    (when options-lines
      (export-org-backend-append context
                                 (export-org-backend-normalize-element
                                  (format nil "~{~A~%~}" options-lines))))
    (when
        (and (export-org-backend-option-enabled-p context :title t)
             (export-org-backend-nonblank-p title))
      (export-org-backend-append context (format nil "#+title: ~A~%" title)))
    (when
        (and (export-org-backend-option-enabled-p context :date t)
             (export-org-backend-nonblank-p date))
      (export-org-backend-append context (format nil "#+date: ~A~%" date)))
    (when
        (and (export-org-backend-option-enabled-p context :author t)
             (export-org-backend-nonblank-p author))
      (export-org-backend-append context (format nil "#+author: ~A~%" author)))
    (when
        (and (export-org-backend-option-enabled-p context :email nil)
             (export-org-backend-nonblank-p email))
      (export-org-backend-append context (format nil "#+email: ~A~%" email)))
    (when
        (and (export-org-backend-option-enabled-p context :creator nil)
             (export-org-backend-nonblank-p creator))
      (export-org-backend-append context (format nil "#+creator: ~A~%" creator)))))

(defun export-org-backend-render-body (context)
  (let* ((document (export-render-context-document context))
         (state (export-org-backend-state context))
         (buffer (export-org-backend-state-buffer state)))
    (setf (fill-pointer buffer) 0
          (export-org-backend-state-base-offset state) 0
          (export-org-backend-state-emitted-footnotes state) (make-hash-table :test 'equal))
    (export-org-backend-render-template-header context)
    (export-org-backend-render-children
     context (export-render-context-root-views context) document)
    (coerce (export-org-backend-state-buffer state) 'string)))

(defun export-org-backend-title-text (node view context)
  (let ((title-macros (getf (%export-view-payload view) :title-macros)))
    (with-output-to-string (output)
      (dolist (segment (node-property node :title))
        (let* ((macro-entry
                 (and (node-p segment)
                      (assoc segment title-macros :test (function eq))))
               (text
                 (cond
                   ((stringp segment) segment)
                   ((node-p segment)
                    (if macro-entry
                        (export-org-backend-capture
                         context
                         (lambda ()
                           (export-org-backend-render-children
                            context (cdr macro-entry))))
                        (node-text segment))))))
          (when text
            (write-string text output))
          (when macro-entry
            (loop repeat (node-post-blank segment)
                  do (write-char #\Space output))))))))

(defun export-org-backend-priority-string (priority)
  (if (<= 0 priority 64)
      (format nil "~D" priority)
      (string (code-char priority))))

(defun export-org-backend-headline-pre-blank (node view)
  (let* ((document (export-org-backend-source-document view))
         (source (and document (document-text document)))
         (start (node-begin node)))
    (if (and source start)
        (let* ((line-end (or (position #\Newline source :start start) (length source)))
               (position
                (if (< line-end (length source))
                    (1+ line-end)
                    line-end))
               (count 0))
          (loop while (< position (length source))
                for newline = (position #\Newline source :start position)
                for
                end = (or newline (length source))
                while (every
                       (lambda (character)
                         (find character '(#\Space #\Tab)))
                       (subseq source position end))
                do (incf count) (setf position (if newline
                                                   (1+ newline)
                                                   end))
                finally (return count))
          count)
        0)))

(defun export-org-backend-render-headline (node view context)
  (export-org-backend-render-element context
                                     (lambda ()
                                       (let* ((level (or (node-property node :level) 1))
                                              (todo (node-property node :todo-keyword))
                                              (priority (node-property node :priority))
                                              (commented-p (node-property node :commentedp))
                                              (tags (node-property node :tags))
                                              (heading (make-string level :initial-element #\*)))
                                         (when (and todo
                                                    (export-org-backend-option-enabled-p context :todo t))
                                           (setf heading (concatenate 'string heading " " todo)))
                                         (when (and priority
                                                    (export-org-backend-option-enabled-p context :priority nil))
                                           (setf heading (concatenate 'string heading " [#"
                                                                      (export-org-backend-priority-string priority)
                                                                      "]")))
                                         (when commented-p
                                           (setf heading (concatenate 'string heading " COMMENT")))
                                         (setf heading (concatenate 'string heading " "
                                                                    (export-org-backend-title-text
                                                                     node view context)))
                                         (when (and tags
                                                    (export-org-backend-option-enabled-p context :tags t))
                                           (let ((tag-text (format nil ":~{~A~^:~}:" tags)))
                                             (setf heading
                                                   (concatenate
                                                    'string heading
                                                    (make-string
                                                     (max 1 (- 77 (length heading)
                                                               (length tag-text)))
                                                     :initial-element #\Space)
                                                    tag-text))))
                                         (export-org-backend-append context heading)
                                         (export-org-backend-append
                                          context
                                          (make-string
                                           (1+ (export-org-backend-headline-pre-blank node view))
                                           :initial-element #\Newline))
                                         (export-org-backend-render-children
                                          context (%export-view-children view)
                                          (export-org-backend-source-document view))))))

(defun export-org-backend-standard-footnote-reference-p (node)
  (and (eq (node-type node) :footnote-reference) (eq (node-property node :type) :standard)))

(defun export-org-backend-view-footnote-labels (views)
  (let ((labels nil))
    (labels ((visit (view)
               (let ((node (%export-view-node view)))
                 (when (export-org-backend-standard-footnote-reference-p node)
                   (push (node-property node :label) labels))
                 (dolist (child (%export-view-children view))
                   (visit child)))))
      (dolist (view views)
        (visit view)))
    (nreverse labels)))

(defun export-org-backend-headline-footnote-labels (headline)
  (when (and headline (node-p headline))
    (loop for segment in (node-property headline :title)
          when (node-p segment)
            append (let ((labels nil))
                     (map-nodes
                      (lambda (node)
                        (when (export-org-backend-standard-footnote-reference-p node)
                          (push (node-property node :label) labels)))
                      segment)
                     (nreverse labels)))))

(defun export-org-backend-section-footnote-labels (node view)
  (let* ((payload (%export-view-payload view))
         (headline (or (getf payload :org-parent-headline) (node-parent node))))
    (append (export-org-backend-headline-footnote-labels headline)
            (export-org-backend-view-footnote-labels (%export-view-children view)))))

(defun export-org-backend-footnote-definition (context label)
  (find label
        (export-render-context-footnotes context)
        :key
        (lambda (entry)
          (getf entry :label))
        :test
        #'equal))

(defun export-org-backend-render-footnotes (node view context)
  (let ((state (export-org-backend-state context)))
    (dolist (label (export-org-backend-section-footnote-labels node view))
      (unless (gethash label (export-org-backend-state-emitted-footnotes state))
        (let ((entry (export-org-backend-footnote-definition context label)))
          (when entry
            (setf (gethash label (export-org-backend-state-emitted-footnotes state)) t)
            (export-org-backend-append context (string #\Newline))
            (export-org-backend-append context (format nil "[fn:~A] " label))
            (let* ((definition (getf entry :definition))
                   (body
                    (export-org-backend-capture context
                                                (lambda ()
                                                  (export-org-backend-render-children context
                                                                                      (%export-view-children
                                                                                       definition)))))
                   (body (export-org-backend-normalize-element body)))
              (export-org-backend-append context body))))))))

(defun export-org-backend-render-section (node view context)
  (export-org-backend-render-element context
                                     (lambda ()
                                       (export-org-backend-render-children
                                        context (%export-view-children view)
                                        (export-org-backend-source-document view))
                                       (let* ((state (export-org-backend-state context))
                                              (contents
                                               (subseq (export-org-backend-state-buffer state)
                                                       0
                                                       (fill-pointer
                                                        (export-org-backend-state-buffer state)))))
                                         (setf (fill-pointer (export-org-backend-state-buffer state)) 0)
                                         (export-org-backend-append
                                          context (export-org-backend-normalize-element contents)))
                                       (export-org-backend-render-footnotes node view context))))

(defun export-org-backend-render-keyword (node view context)
  (let* ((key (string-upcase (or (node-property node :key) "")))
         (value (or (node-property node :value) "")))
    (cond
      ((string= key "INCLUDE")
       (export-org-backend-render-element
        context
        (lambda ()
          (export-org-backend-render-children
           context (%export-view-children view)))))
      ((string= key "RESULTS")
       (export-org-backend-render-object
        context
        (lambda ()
          (export-org-backend-append context "#+results: ")
          (export-org-backend-append context value)
          (export-org-backend-append context (string #\Newline)))) )
      ((member key '("AUTHOR" "CREATOR" "DATE" "EMAIL" "OPTIONS" "TITLE") :test #'string=) "")
      (t
       (export-org-backend-render-element
        context
        (lambda ()
          (export-org-backend-append
           context (format nil "#+~A: ~A" (string-downcase key) value))))))))

(defun export-org-backend-render-export-block (node context)
  (if (string= (or (node-property node :type) "") "ORG")
      (export-org-backend-render-element
       context
       (lambda ()
         (export-org-backend-append
          context (or (node-property node :value) ""))))
      ""))

(defun export-org-backend-render-footnote-definition (node context)
  (declare (ignore node context))
  "")
(defun export-org-backend-render-timestamp (node view context)
  (let* ((type (node-property node :type))
         (range-p (or (eq type :active-range)
                      (eq type :inactive-range)))
         (opening (if (or (eq type :inactive)
                          (eq type :inactive-range))
                      #\[
                      #\<))
         (closing (if (or (eq type :inactive)
                          (eq type :inactive-range))
                      #\]
                      #\>))
         (adjustments (emitter-object-timestamp-adjustments node)))
    (if (eq type :diary)
        (export-org-backend-render-identity node view context)
        (labels ((date-string (year-key month-key day-key)
                   (let* ((year (node-property node year-key))
                          (month (node-property node month-key))
                          (day (node-property node day-key))
                          (weekday
                            (case (cl-date-kit:day-of-week
                                   (cl-date-kit:make-local-date
                                    year month day))
                              (:monday "Mon")
                              (:tuesday "Tue")
                              (:wednesday "Wed")
                              (:thursday "Thu")
                              (:friday "Fri")
                              (:saturday "Sat")
                              (:sunday "Sun"))))
                     (format nil "~4,'0D-~2,'0D-~2,'0D ~A"
                             year month day weekday)))
                 (time-string (hour-key minute-key)
                   (let ((hour (node-property node hour-key))
                         (minute (node-property node minute-key)))
                     (when (and hour minute)
                       (format nil "~2,'0D:~2,'0D" hour minute))))
                 (endpoint (year-key month-key day-key hour-key minute-key)
                   (format nil "~C~A~@[ ~A~]~A~C"
                           opening
                           (date-string year-key month-key day-key)
                           (time-string hour-key minute-key)
                           adjustments
                           closing)))
          (let* ((start-date
                   (date-string :year-start :month-start :day-start))
                 (end-date
                   (date-string :year-end :month-end :day-end))
                 (start-time (time-string :hour-start :minute-start))
                 (end-time (time-string :hour-end :minute-end)))
            (cond
              ((not range-p)
               (endpoint :year-start :month-start :day-start
                         :hour-start :minute-start))
              ((and (string= start-date end-date) start-time end-time)
               (format nil "~C~A ~A-~A~A~C"
                       opening start-date start-time end-time
                       adjustments closing))
              (t
               (format nil "~A--~A"
                       (endpoint :year-start :month-start :day-start
                                 :hour-start :minute-start)
                       (endpoint :year-end :month-end :day-end
                                 :hour-end :minute-end)))))))))
(defun export-org-backend-render-link (node view frame context)
  (let ((result
          (export-call-link-type-handler-for-view
           context node view frame)))
    (if result
        (let* ((destination (export-link-result-destination result))
               (description (export-link-result-description result))
               (children (%export-view-children view))
               (rendered-description
                 (if (string=
                      description
                      (export-view-plain-text-description view))
                     (export-org-backend-capture
                      context
                      (lambda ()
                        (export-org-backend-render-children context children)))
                     description)))
          (format nil
                  "[[~A]~A]"
                  (emitter-object-link-escape-path destination)
                  (if children
                      (format nil "[~A]" rendered-description)
                      "")))
        (export-org-backend-render-identity node view context))))

(defun export-org-backend-transcode (node view frame context)
  (case (node-type node)
    (:headline (export-org-backend-render-headline node view context))
    (:section (export-org-backend-render-section node view context))
    (:keyword (export-org-backend-render-keyword node view context))
    (:link (export-org-backend-render-link node view frame context))
    (:timestamp (export-org-backend-render-timestamp node view context))
    (:planning
     (if (export-org-backend-option-enabled-p context :planning nil)
         (export-org-backend-render-identity node view context)
         ""))
    (:clock
     (if (export-org-backend-option-enabled-p context :clock nil)
         (export-org-backend-render-identity node view context)
         ""))
    (:property-drawer
     (if (export-org-backend-option-enabled-p context :properties nil)
         (let* ((selection (export-option-value context :properties))
                (children (%export-view-children view))
                (selected-children
                  (if (consp selection)
                      (remove-if-not
                       (lambda (child-view)
                         (export-option-selects-name-p
                          selection
                          (node-property (%export-view-node child-view) :key)))
                       children)
                      children)))
           (export-org-backend-render-identity
            node
            (if (eq selected-children children)
                view
                (export-org-backend-copy-view view selected-children))
            context))
         ""))
    (:drawer
     (let* ((policy (export-option-value context :drawers))
            (name (node-property node :drawer-name)))
       (if (or (eq policy t)
               (and (consp policy)
                    (export-option-selects-name-p policy name)))
           (export-org-backend-render-identity node view context)
           "")))
    (:export-block (export-org-backend-render-export-block node context))
    (:footnote-definition
     (export-org-backend-render-footnote-definition node context))
    (:inline-src-block "")
    (otherwise (export-org-backend-render-identity node view context))))

(defun export-org-backend-copy-view (view children)
  (%make-export-view :node
                     (%export-view-node view)
                     :children
                     children
                     :payload
                     (%export-view-payload view)
                     :source-document
                     (%export-view-source-document view)))

(defun export-org-backend-add-missing-sections (views context)
  (declare (ignore context))
  (labels ((rebuild (view)
             (let* ((node (%export-view-node view))
                    (children (mapcar #'rebuild (%export-view-children view))))
               (when
                   (and (eq (node-type node) :headline)
                        (not
                         (and children
                              (eq (node-type (%export-view-node (first children))) :section))))
                 (let* ((section (make-node :section))
                        (section-view
                         (%make-export-view :node
                                            section
                                            :children
                                            nil
                                            :payload
                                            (list :org-parent-headline node)
                                            :source-document
                                            (%export-view-source-document view))))
                   (setf children (cons section-view children))))
               (export-org-backend-copy-view view children))))
    (mapcar #'rebuild views)))

(defun export-org-backend-transcoders ()
  (mapcar
   (lambda (type)
     (cons type #'export-org-backend-transcode))
   (node-types)))

(defun export-org-backend-option-specifications ()
  (list (list :token "special-rows" :key :with-special-rows
              :parser :boolean :default t)
        (list :token "cite-processors" :key :with-cite-processors
              :parser :boolean :default nil)
        (list :token "d" :key :drawers :parser :drawer-list
              :default '(:exclude "LOGBOOK"))
        (list :token "p" :key :planning :parser :boolean :default nil)
        (list :token "pri" :key :priority :parser :boolean :default nil)
        (list :token "prop" :key :properties :parser :selection-list
              :default nil)
        (list :token "c" :key :clock :parser :boolean :default nil)))

(defun export-org-backend-keywords ()
  '("TITLE" "AUTHOR"
    "CREATOR"
    "DATE"
    "EMAIL"
    "HTML"
    "OPTIONS"
    "EXCLUDE_TAGS"
    "LANGUAGE"
    "TODO"
    "SEQ_TODO"
    "TYP_TODO"
    "PROPERTY"
    "FILETAGS"
    "STARTUP"
    "SETUPFILE"
    "BIND"
    "LINK"
    "MACRO"
    "NAME"
    "CAPTION"
    "HEADER"
    "HEADERS"
    "PLOT"
    "RESULTS"
    "ATTR_HTML"
    "ATTR_ORG"
    "ATTR_LATEX"
    "COLUMNS"
    "PRIORITIES"
    "CONSTANTS"
    "ARCHIVE"
    "CATEGORY"
    "DESCRIPTION"
    "KEYWORDS"
    "SUBTITLE"
    "EXPORT_FILE_NAME"
    "CITE_EXPORT"
    "BIBLIOGRAPHY"
    "PRINT_BIBLIOGRAPHY"))

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend :org
                           :renderer
                           #'export-org-backend-render-body
                           :make-state
                           (lambda (context)
                             (export-org-backend-make-state :limits
                                                            (export-render-context-limits context)))
                           :transcoders
                           (export-org-backend-transcoders)
                           :option-specifications
                           (export-org-backend-option-specifications)
                           :accepted-keywords
                           (export-org-backend-keywords)
                           :retained-keywords
                           (export-org-backend-keywords)
                           :filters
                           (list
                            (cons :parse-tree (list #'export-org-backend-add-missing-sections)))))
