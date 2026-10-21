;;;; src/export-texinfo.lisp
(in-package #:org-kit)

(defstruct (export-texinfo-state (:constructor %make-export-texinfo-state))
  preparation
  headlines
  node-names)

(defun export-texinfo-title-source (node)
  (or (node-property node :raw-value)
      (node-property node :value)
      ""))

(defun export-texinfo-node-name-base (text)
  (let ((text (with-output-to-string (output)
                (loop with space-p = nil
                      for character across text
                      do (cond
                           ((find character (list #\Space #\Tab #\Return #\Newline))
                            (unless space-p (write-char #\Space output))
                            (setf space-p t))
                           ((find character ",:.")
                            (setf space-p nil))
                           (t
                            (write-char character output)
                            (setf space-p nil)))))))
    (string-trim '(#\Space) text)))

(defun export-texinfo-index-headlines (context)
  (let ((headlines nil)
        (node-names (make-hash-table :test #'eq))
        (used-names (make-hash-table :test #'equal)))
    (labels ((visit (view)
               (let* ((node (export-view-node view))
                      (type (node-type node)))
                 (when (eq type :headline)
                   (push node headlines)
                   (let* ((base (export-texinfo-node-name-base
                                 (export-texinfo-title-source node)))
                          (base (if (plusp (length base)) base "Untitled"))
                          (name base)
                          (suffix 0))
                     (loop while (or (string-equal name "Top")
                                     (gethash name used-names))
                           do (incf suffix)
                              (setf name (format nil "~A (~D)" base suffix)))
                     (setf (gethash name used-names) t
                           (gethash node node-names) name)))
                 (dolist (child (export-view-children view))
                   (visit child)))))
      (dolist (view (export-render-context-root-views context))
        (visit view)))
    (%make-export-texinfo-state
     :preparation (org-kit::%export-render-context-preparation context)
     :headlines (nreverse headlines)
     :node-names node-names)))

(defun export-texinfo-make-state (context)
  (export-texinfo-index-headlines context))

(defun export-texinfo-check-length (context offset length phase)
  (export-check-output-actual (+ offset length)
                              (export-render-context-limits context)
                              (export-render-context-backend context)
                              phase :kind :texinfo-output))

(defun export-texinfo-escape-length (text)
  (loop for character across text
        sum (case character
              ((#\@ #\{ #\}) 2)
              (#\, 8)
              (t 1))))

(defun export-texinfo-escape-text (context text &optional (offset *export-current-output-offset*))
  (export-texinfo-check-length context offset
                               (export-texinfo-escape-length text)
                               *export-current-export-phase*)
  (with-output-to-string (output)
    (loop for character across text
          do (case character
               (#\@ (write-string "@@" output))
               (#\{ (write-string "@{" output))
               (#\} (write-string "@}" output))
               (#\, (write-string "@comma{}" output))
               (t (write-char character output))))))

(defun export-texinfo-concat (context offset phase &rest strings)
  (let ((length (loop for string in strings when string sum (length string))))
    (export-texinfo-check-length context offset length phase)
    (apply #'concatenate 'string (remove nil strings))))

(defun export-texinfo-child-views (view)
  (and view (export-view-children view)))

(defun export-texinfo-render-children (context view frame &optional mode)
  (let ((offset (export-render-frame-output-offset frame))
        (parent-node (export-view-node view))
        (mode (or mode (export-render-frame-mode frame)))
        (previous-text nil)
        (previous-source-document nil))
    (with-output-to-string (output)
      (dolist (child (export-texinfo-child-views view))
        (let* ((*export-current-output-offset* offset)
               (node (export-view-node child))
               (source-document (export-view-source-document child))
               (text (export-render-view context child :mode mode
                                         :preserve-source-breaks-p
                                         (export-render-frame-preserve-source-breaks-p
                                          frame))))
          (when (and (eq mode :inline)
                     (not (eq (node-type-category (node-type node)) :object)))
            (setf text (export-node-strip-post-blank node text)))
          (when (and (eq mode :block)
                     (eq (node-type parent-node) :section)
                     (or (eq (node-type-category (node-type node)) :greater-element)
                         (eq (node-type-category (node-type node)) :lesser-element))
                     (plusp (length text)))
            (let* ((base (export-node-strip-post-blank node text))
                   (post-blank (subseq text (length base))))
              (if (zerop (length base))
                  (setf text "")
                (progn
                  (unless (char= (char base (1- (length base))) #\Newline)
                    (setf base
                          (export-texinfo-concat
                           context offset *export-current-export-phase*
                           base (string #\Newline))))
                  (setf text
                        (export-texinfo-concat
                         context offset *export-current-export-phase*
                         base post-blank))))))
          (when (and (eq mode :block)
                     (plusp (length text))
                     previous-text
                     (plusp (length previous-text))
                     previous-source-document
                     source-document
                     (not (eq previous-source-document source-document)))
            (let* ((last-non-newline
                     (position-if-not
                      (lambda (character) (char= character #\Newline))
                      previous-text :from-end t))
                   (trailing-newlines
                     (- (length previous-text)
                        (if last-non-newline (1+ last-non-newline) 0)))
                   (first-non-newline
                     (position-if-not
                      (lambda (character) (char= character #\Newline))
                      text))
                   (leading-newlines (or first-non-newline (length text)))
                   (separator-count
                     (max 0 (- 2 trailing-newlines leading-newlines))))
              (when (plusp separator-count)
                (setf text
                      (export-texinfo-concat
                       context offset *export-current-export-phase*
                       (make-string separator-count :initial-element #\Newline)
                       text)))))
          (export-texinfo-check-length context offset (length text)
                                       *export-current-export-phase*)
          (write-string text output)
          (incf offset (length text))
          (setf previous-text text
                previous-source-document source-document))))))

(defun export-texinfo-node-value (node)
  (or (node-property node :value)
      (node-property node :raw-value)
      (node-text node)
      ""))

(defun export-texinfo-render-inline (context view frame)
  (export-texinfo-render-children context view frame :inline))

(defun export-texinfo-property-records (context key)
  (loop for record in (export-keyword-records
                       (export-render-context-document context))
        when (string= (second record) key)
          collect (third record)))

(defun export-texinfo-join-normalized-lines (texts)
  (apply #'concatenate 'string
         (mapcar #'export-texinfo-normalize-lines texts)))

(defun export-texinfo-single-property (context key)
  (car (last (export-texinfo-property-records context key))))

(defun export-texinfo-unquote (text)
  (if (and (>= (length text) 2)
           (char= (char text 0) #\")
           (char= (char text (1- (length text))) #\"))
      (subseq text 1 (1- (length text)))
      text))

(defun export-texinfo-normalize-lines (text)
  (if (stringp text)
      (let ((text (string-trim '(#\Newline #\Return) text)))
        (if (zerop (length text)) "" (concatenate 'string text (string #\Newline))))
      ""))

(defun export-texinfo-render-plain-text (context node frame)
  (let* ((value (export-texinfo-node-value node))
         (value
          (with-output-to-string (output)
            (loop for character across value
                  do (if (char= character #\Tab)
                         (dotimes (_ 8)
                           (write-char #\Space output))
                       (write-char character output)))))
         (escaped (export-texinfo-escape-text
                   context value (export-render-frame-output-offset frame))))
    (export-texinfo-concat context (export-render-frame-output-offset frame)
                           *export-current-export-phase* escaped)))

(defun export-texinfo-render-markup (context node view frame wrapper &optional escape)
  (let* ((contents (export-texinfo-render-inline context view frame))
         (contents
           (if escape
               (let* ((value (export-texinfo-node-value node))
                      (delimiter
                        (case (node-type node)
                          (:code #\~)
                          (:verbatim #\=)))
                      (end (position-if-not
                            (lambda (character)
                              (member character
                                      '(#\Space #\Tab #\Newline #\Return)))
                            value :from-end t))
                      (value (if (and delimiter end (> end 0)
                                      (char= (char value 0) delimiter)
                                      (char= (char value end) delimiter))
                                 (subseq value 1 end)
                                 value)))
                 (export-texinfo-escape-text
                  context value (export-render-frame-output-offset frame)))
               contents)))
    (export-texinfo-concat context (export-render-frame-output-offset frame)
                           *export-current-export-phase*
                           (format nil wrapper contents)))
)

(defun export-texinfo-entity (node)
  (let* ((name (node-property node :name))
         (entry (assoc name
                       '(("AElig" . "@AE{}") ("aelig" . "@ae{}")
                         ("bull" . "@bullet{}") ("bullet" . "@bullet{}")
                         ("copy" . "@copyright{}") ("deg" . "@textdegree{}")
                         ("dots" . "@dots{}") ("hellip" . "@dots{}")
                         ("equiv" . "@equiv{}") ("euro" . "@euro{}")
                         ("ge" . "@geq{}") ("geq" . "@geq{}")
                         ("laquo" . "@guillemetleft{}")
                         ("iexcl" . "@exclamdown{}") ("iquest" . "@questiondown{}")
                         ("le" . "@leq{}") ("leq" . "@leq{}")
                         ("mdash" . "---") ("minus" . "@minus{}")
                         ("nbsp" . "@tie{}") ("ndash" . "--")
                         ("OElig" . "@OE{}") ("oelig" . "@oe{}")
                         ("ordf" . "@ordf{}") ("ordm" . "@ordm{}"))
                       :test #'string=)))
    (or (cdr entry)
        (node-property node :utf-8)
        (node-property node :html)
        (export-texinfo-node-value node))))

(defun export-texinfo-headline-node-name (context node)
  (or (gethash node (export-texinfo-state-node-names
                     (export-render-context-state context)))
      (export-texinfo-node-name-base (export-texinfo-title-source node))))

(defun export-texinfo-node-headlines (context parent)
  (remove-if-not
   (lambda (node) (eq (node-parent node) parent))
   (export-texinfo-state-headlines (export-render-context-state context))))

(defun export-texinfo-menu-entry (context node)
  (let* ((title (string-trim
                 '(#\Space #\Tab)
                 (remove #\: (export-texinfo-title-source node))))
         (node-name (export-texinfo-headline-node-name context node)))
    (if (string= title node-name)
        (format nil "* ~A::~%" title)
        (format nil "* ~A: ~A. ~%" title node-name))))

(defun export-texinfo-menu (context nodes offset &optional master)
  (when nodes
    (labels ((menu-entries (headlines)
               (format nil "~{~A~^~%~}" (mapcar
                        (lambda (node)
                          (string-right-trim
                           '(#\Newline #\Return #\Space)
                           (export-texinfo-menu-entry context node)))
                        headlines)))
             (detail-entries (headlines depth)
               (if (zerop depth)
                   ""
                   (with-output-to-string (output)
                     (dolist (node headlines)
                       (let ((children
                               (export-texinfo-node-headlines context node)))
                         (when children
                           (format output "~A~%~%~A~%~%"
                                   (export-texinfo-title-source node)
                                   (menu-entries children))
                           (write-string
                            (detail-entries children (1- depth))
                            output))))))))
      (let* ((detail-depth
               (let ((toc (export-texinfo-option context :toc t)))
                 (if (and (integerp toc) (not (minusp toc))) toc 4)))
             (detail-text (and master (detail-entries nodes detail-depth)))
             (detail-p (and detail-text (plusp (length detail-text)))))
        (if detail-p
            (export-texinfo-concat
             context offset *export-current-export-phase*
             "@menu" (string #\Newline)
             (menu-entries nodes)
             (format nil "~%~%@detailmenu~%--- The Detailed Node Listing ---~%~%")
             detail-text
             (format nil "@end detailmenu~%@end menu~%"))
            (export-texinfo-concat
             context offset *export-current-export-phase*
             "@menu" (string #\Newline)
             (menu-entries nodes)
             (format nil "~%@end menu~%")))))
))

(defun export-texinfo-section-command (level numbered-p)
  (let ((commands
          (if numbered-p
              '("chapter" "section" "subsection" "subsubsection")
              '("unnumbered" "unnumberedsec" "unnumberedsubsec"
                "unnumberedsubsubsec"))))
    (nth (1- (max 1 (min level (length commands)))) commands)))

(defun export-texinfo-render-headline (context node view frame)
  (let* ((offset (export-render-frame-output-offset frame))
         (level (max 1 (or (node-property node :level) 1)))
         (numbering
           (let ((entry (assoc :num (export-render-context-options context))))
             (if entry (cdr entry) t)))
         (source-title (export-texinfo-title-source node))
         (tags (node-property node :tags))
         (title (if (and tags (export-texinfo-option context :tags t))
                    (format nil "~A :~{~A~^:~}:"
                            source-title tags)
                    source-title))
         (title (export-texinfo-escape-text context title offset))
         (name (export-texinfo-headline-node-name context node))
         (command (export-texinfo-section-command level numbering))
         (contents (export-texinfo-render-children context view frame :block))
         (prefix (format nil "@node ~A~%@~A ~A~%~%"
                         name command title))
         (text (export-texinfo-concat context offset
                                      *export-current-export-phase*
                                      prefix contents)))
    (if (and (plusp (length text))
             (not (char= (char text (1- (length text))) #\Newline)))
        (export-texinfo-concat context (+ offset (length text))
                               *export-current-export-phase*
                               text (string #\Newline))
        text))
)

(defun export-texinfo-render-plain-list (context node view frame)
  (let* ((offset (export-render-frame-output-offset frame))
         (type (node-property node :type))
         (list-type (case type
                      (:ordered "enumerate")
                      (:unordered "itemize")
                      (otherwise "table")))
         (contents (export-texinfo-render-children context view frame :block)))
    (export-texinfo-concat context offset *export-current-export-phase*
                           (format nil "@~A~%" list-type)
                           contents (format nil "@end ~A" list-type))))

(defun export-texinfo-render-item (context node view frame)
  (let* ((offset (export-render-frame-output-offset frame))
         (tag (node-property node :tag))
         (contents (export-texinfo-render-children context view frame :block))
         (tag-text (if (stringp tag) (export-texinfo-escape-text context tag offset) "")))
    (export-texinfo-concat context offset *export-current-export-phase*
                           (if (zerop (length tag-text)) "@item" (format nil "@item ~A" tag-text))
                           (string #\Newline) contents (string #\Newline))))

(defun export-texinfo-render-link (context node view frame)
  (let* ((offset (export-render-frame-output-offset frame))
         (type (or (node-property node :type) ""))
         (path (or (node-property node :path) ""))
         (target (and (member type '("fuzzy" "custom-id") :test #'string=)
                      (find-if
                       (lambda (headline)
                         (or (and (string= type "fuzzy")
                                  (string-equal
                                   path (export-texinfo-title-source headline)))
                             (multiple-value-bind (custom-id sources)
                                 (effective-property
                                  (node-document headline) headline "CUSTOM_ID")
                               (declare (ignore sources))
                               (and custom-id
                                    (string-equal path custom-id)))))
                       (export-texinfo-state-headlines
                        (export-render-context-state context)))))
         (description (export-texinfo-render-inline context view frame))
         (destination (export-texinfo-escape-text
                       context (if (string= type "file")
                                   (concatenate 'string "file://" path)
                                   (concatenate 'string type ":" path))
                       offset)))
    (cond
      ((string= type "mailto")
       (export-texinfo-concat context offset *export-current-export-phase*
                              (format nil "@email{~A~A}" path
                                      (if (plusp (length description))
                                          (concatenate 'string ", " description) ""))))
      ((member type '("fuzzy" "custom-id" "id" "radio") :test #'string=)
       (export-texinfo-concat
        context offset *export-current-export-phase*
        (format nil "@ref{~A~A}"
                (if target (export-texinfo-headline-node-name context target) path)
                (if (plusp (length description))
                    (format nil ", , ~A" description) ""))))
      (t
       (export-texinfo-concat context offset *export-current-export-phase*
                              (format nil "@uref{~A~A}" destination
                                      (if (plusp (length description))
                                          (format nil ", ~A" description) "")))))))

(defun export-texinfo-render-block (context node view frame command)
  (let* ((offset (export-render-frame-output-offset frame))
         (value (export-texinfo-node-value node))
         (value (if (and (plusp (length value))
                         (not (char= (char value (1- (length value))) #\Newline)))
                    (export-texinfo-concat context offset *export-current-export-phase*
                                           value (string #\Newline))
                  value))
         (value (export-texinfo-escape-text context value offset))
         (code (export-texinfo-concat
                context offset *export-current-export-phase*
                (format nil "@~A~%~A@end ~A" command value command))))
    (if (not (eq (node-type node) :src-block))
        code
      (let* ((mode (getf (export-view-payload view) :exports))
             (results (export-texinfo-render-children
                       context view frame :block)))
        (case mode
          (:none "")
          (:results results)
          (:both
           (if (zerop (length results))
               code
             (export-texinfo-concat
              context offset *export-current-export-phase*
              code (format nil "~%~%") results)))
          (otherwise code)))))
  )

(defun export-texinfo-render-section (context node view frame)
  (let* ((offset (export-render-frame-output-offset frame))
         (contents (export-texinfo-render-children context view frame (if (eq (export-render-frame-mode frame) :inline) :inline :block)))
         (parent (node-parent node)))
    (cond
     ((and parent (eq (node-type parent) :headline))
      (let ((menu (export-texinfo-menu
                   context (export-texinfo-node-headlines context parent)
                   (+ offset (length contents)))))
        (if menu
            (let* ((end (length contents))
                   (trailing-newlines
                    (progn
                      (loop while (and (plusp end)
                                       (char= (char contents (1- end)) #\Newline))
                            do (decf end))
                      (- (length contents) end)))
                   (missing (max 0 (- 2 trailing-newlines)))
                   (contents (if (plusp missing)
                                 (export-texinfo-concat
                                  context offset *export-current-export-phase*
                                  contents
                                  (make-string missing :initial-element #\Newline))
                               contents)))
              (export-texinfo-concat context offset *export-current-export-phase*
                                     contents menu (string #\Newline)))
          contents)))
     ((and (export-view-source-document view)
           (not (eq (export-view-source-document view)
                    (export-render-context-document context))))
      contents)
     (t ""))))

(defun export-texinfo-transcode-node (node view frame context)
  (let* ((offset (export-render-frame-output-offset frame))
         (type (node-type node)))
    (case type
      (:plain-text (export-texinfo-render-plain-text context node frame))
      (:bold (export-texinfo-render-markup context node view frame "@strong{~A}"))
      (:italic (export-texinfo-render-markup context node view frame "@emph{~A}"))
      ((:code :inline-src-block)
       (export-texinfo-render-markup context node view frame "@code{~A}" t))
      (:verbatim
       (export-texinfo-render-markup context node view frame "@samp{~A}" t))
      (:strike-through (export-texinfo-render-inline context view frame))
      (:underline (export-texinfo-render-inline context view frame))
      (:subscript
       (format nil "@math{_~A}" (export-texinfo-render-inline context view frame)))
      (:superscript
       (format nil "@math{^~A}" (export-texinfo-render-inline context view frame)))
      (:entity (export-texinfo-entity node))
      (:line-break (format nil "@*~%"))
      (:paragraph
       (string-trim '(#\Newline #\Return)
                    (export-texinfo-render-children context view frame :inline)))
      (:headline (export-texinfo-render-headline context node view frame))
      (:section (export-texinfo-render-section context node view frame))
      (:plain-list (export-texinfo-render-plain-list context node view frame))
      (:item (export-texinfo-render-item context node view frame))
      (:link (export-texinfo-render-link context node view frame))
      (:target (format nil "@anchor{~A}" (or (node-property node :value) "")))
      (:radio-target
       (export-texinfo-concat context offset *export-current-export-phase*
                              (format nil "@anchor{~A}" (or (node-property node :value) ""))
                              (export-texinfo-render-inline context view frame)))
      ((:example-block :fixed-width)
       (export-texinfo-render-block context node view frame "example"))
      (:src-block
       (export-texinfo-render-block
        context node view frame
        (if (search "lisp" (or (node-property node :language) "")
                    :test #'char-equal)
            "lisp" "example")))
      (:quote-block
       (export-texinfo-concat context offset *export-current-export-phase*
                              "@quotation" (string #\Newline)
                              (export-texinfo-render-children context view frame :block)
                              (format nil "~%~%") "@end quotation"))
      (:verse-block
       (export-texinfo-concat context offset *export-current-export-phase*
                              "@display" (string #\Newline)
                              (export-texinfo-render-children context view frame :block)
                              "@end display"))
      (:center-block
       (let ((contents (export-texinfo-render-children context view frame :block)))
         (if (zerop (length contents)) ""
           (export-texinfo-concat context offset *export-current-export-phase*
                                  "@center " contents))))
      (:special-block
       (let* ((name (or (node-property node :type) "quotation"))
              (contents (export-texinfo-render-children context view frame :block)))
         (export-texinfo-concat context offset *export-current-export-phase*
                                (format nil "@~A~%" name) contents
                                (format nil "@end ~A" name))))
      (:export-block
       (if (string-equal (or (node-property node :type) "") "TEXINFO")
           (or (node-property node :value) "")
         ""))
      (:export-snippet
       (if (string-equal (or (node-property node :back-end) "") "texinfo")
           (or (node-property node :value) "")
         ""))
      (:keyword
       (let ((key (string-upcase (or (node-property node :key) "")))
             (value (or (node-property node :value) "")))
         (cond ((string= key "TEXINFO") value)
               ((member key '("CINDEX" "FINDEX" "KINDEX" "PINDEX" "TINDEX" "VINDEX")
                        :test #'string=)
                (format nil "@~A ~A" (string-downcase key) value))
               (t ""))))
      (:footnote-reference
       (let* ((label (node-property node :label))
              (entry (and label
                          (find label
                                (export-preparation-footnotes
                                 (export-texinfo-state-preparation
                                  (export-render-context-state context)))
                                :key (lambda (item) (getf item :label))
                                :test #'equal)))
              (definition (and entry (getf entry :definition))))
         (cond
          (definition
           (let ((contents
                  (export-texinfo-render-children
                   context definition frame :block)))
             (export-texinfo-concat
              context offset *export-current-export-phase*
              "@footnote{" contents "}")))
          ((eq (node-property node :type) :inline)
           (let ((contents
                  (export-texinfo-render-children
                   context view frame :inline)))
             (export-texinfo-concat
              context offset *export-current-export-phase*
              "@footnote{" contents "}")))
          (t ""))))
      (:footnote-definition "")
      (:timestamp
       (format nil "@emph{~A}" (export-texinfo-escape-text
                                context (export-texinfo-node-value node) offset)))
      (:statistics-cookie (export-texinfo-node-value node))
      (:node-property
       (format nil "~A: ~A" (or (node-property node :key) "")
               (or (node-property node :value) "")))
      (:property-drawer
       (let ((contents (export-texinfo-render-children context view frame :block)))
         (if (plusp (length contents))
             (format nil "@verbatim~%@~A@end verbatim" contents)
           "")))
      (:drawer (export-texinfo-render-children context view frame :block))
      (:dynamic-block (export-texinfo-render-children context view frame :block))
      (:planning "")
      (:clock "")
      (:latex-environment "")
      (:latex-fragment "")
      (:inlinetask
       (export-texinfo-render-children context view frame :block))
      (:template "")
      (otherwise
       (if (eq type :table)
           (let* ((all-rows (export-view-children view))
                  (rows (remove-if
                         (lambda (row)
                           (eq (node-property (export-view-node row) :type) :rule))
                         all-rows))
                  (column-count
                   (loop for row in rows
                         maximize (length (export-view-children row))
                         into maximum
                         finally (return (or maximum 0))))
                  (header-p
                   (some (lambda (row)
                           (eq (node-property (export-view-node row) :type) :rule))
                         all-rows))
                  (widths
                   (loop for column below column-count
                         collect
                         (loop with width = 1
                               for row in rows
                               for cell = (nth column (export-view-children row))
                               when cell
                               do (setf width
                                        (max width
                                             (length
                                              (export-view-plain-text-description
                                               cell))))
                               finally (return width)))))
             (if (zerop column-count)
                 ""
               (let ((text
                      (with-output-to-string (output)
                        (write-string "@multitable" output)
                        (dolist (width widths)
                          (format output " {~A}" (make-string width :initial-element #\a)))
                        (write-char #\Newline output)
                        (loop for row in rows
                              for row-index from 0
                              do (write-string
                                  (if (and header-p (zerop row-index))
                                      "@headitem " "@item ")
                                  output)
                              (loop for cell in (export-view-children row)
                                    for cell-index from 0
                                    do (when (plusp cell-index)
                                         (write-char #\Newline output)
                                         (write-string "@tab " output))
                                    (write-string
                                     (export-texinfo-render-children
                                      context cell frame :inline)
                                     output))
                              (write-char #\Newline output))
                        (write-string "@end multitable" output))))
                 (export-texinfo-check-length
                  context offset (length text) *export-current-export-phase*)
                 text)))
         (export-texinfo-render-children context view frame))))))

(defun export-texinfo-render-body (context)
  (multiple-value-bind (preamble-section body-section original-section)
      (export-texinfo-top-section context)
    (declare (ignore preamble-section))
    (let ((offset *export-current-output-offset*))
      (with-output-to-string (output)
        (dolist (view (export-render-context-root-views context))
          (let* ((*export-current-output-offset* offset)
                 (frame (%make-export-render-frame :mode :block
                                                   :output-offset offset))
                 (text
                  (if (eq view original-section)
                      (concatenate
                       'string
                       (if body-section
                           (export-texinfo-render-children
                            context body-section frame :block)
                         "")
                       (export-node-post-blank-string
                        (export-view-node view)))
                    (export-render-view context view :mode :block))))
            (export-texinfo-check-length context offset (length text) :body)
            (write-string text output)
            (incf offset (length text)))))))
  )

(defun export-texinfo-top-section (context)
  (let ((view
         (find-if (lambda (view)
                    (let* ((node (export-view-node view))
                           (parent (node-parent node)))
                      (and (eq (node-type node) :section)
                           (or (null parent)
                               (not (eq (node-type parent) :headline))))))
                  (export-render-context-root-views context))))
    (when view
      (let* ((children (export-view-children view))
             (split-at
              (position-if (lambda (child)
                             (eq (node-type (export-view-node child)) :headline))
                           children))
             (node (export-view-node view))
             (payload (export-view-payload view))
             (source-document (export-view-source-document view)))
        (values
         (%make-export-view
          :node node :children (subseq children 0 (or split-at (length children)))
          :payload payload :source-document source-document)
         (when split-at
           (%make-export-view :node node :children (subseq children split-at)
                              :payload payload :source-document source-document))
         view)))))

(defun export-texinfo-option (context key default)
  (let ((entry (assoc key (export-render-context-options context))))
    (if entry (cdr entry) default)))

(defun export-texinfo-render-template (context _preamble body _trailer)
  (declare (ignore _preamble _trailer))
  (let* ((metadata (export-render-context-metadata context))
         (title (or (getf metadata :title) ""))
         (author (getf metadata :author))
         (language (or (getf metadata :language) "en"))
         (filename (export-texinfo-unquote
                    (or (export-texinfo-single-property context "TEXINFO_FILENAME") "")))
         (class (or (export-texinfo-single-property context "TEXINFO_CLASS") "info"))
         (header (export-texinfo-join-normalized-lines
                  (export-texinfo-property-records context "TEXINFO_HEADER")))
         (post-header (export-texinfo-join-normalized-lines
                       (export-texinfo-property-records context
                                                        "TEXINFO_POST_HEADER")))
         (dir-category (or (export-texinfo-single-property context
                                                           "TEXINFO_DIR_CATEGORY")
                           "Misc"))
         (dir-name (or (export-texinfo-single-property context "TEXINFO_DIR_NAME")
                       (export-texinfo-single-property context "TEXINFO_DIR_TITLE")))
         (dir-desc (or (export-texinfo-single-property context "TEXINFO_DIR_DESC") title))
         (printed-title (or (export-texinfo-single-property context
                                                            "TEXINFO_PRINTED_TITLE")
                            title))
         (toc (export-texinfo-option context :toc t))
         (with-title (export-texinfo-option context :title t))
         (with-author (export-texinfo-option context :author t))
         (master-menu (export-texinfo-menu
                       context
                       (remove-if
                        (lambda (node)
                          (loop for parent = (node-parent node)
                                then (node-parent parent)
                                while parent
                                thereis (eq (node-type parent) :headline)))
                        (export-texinfo-state-headlines
                         (export-render-context-state context)))
                       0 t))
         (top-section (export-texinfo-top-section context))
         (offset 0))
    (unless (string= class "info")
      (export-unsupported-syntax :texinfo (export-render-context-document context)
                                 nil :texinfo-class
                                 "Only the built-in info Texinfo class is supported"))
    (with-output-to-string (output)
      (labels ((emit (text)
                     (when text
                       (export-check-output-stage
                        text (export-render-context-limits context) :texinfo :template
                        :base-offset offset :kind :texinfo-template)
                       (write-string text output)
                       (incf offset (length text)))))
        (emit (format nil "\\input texinfo    @c -*- texinfo -*-~%"))
        (emit (format nil "@c %**start of header~%"))
        (when (plusp (length filename))
          (emit (format nil "@setfilename ~A~%" filename)))
        (emit (format nil "@settitle ~A~%" (export-texinfo-escape-text context title offset)))
        (emit (format nil "@documentencoding UTF-8~%@documentlanguage ~A~%"
                      (export-texinfo-escape-text context language offset)))
        (emit header)
        (emit (format nil "@c %**end of header~%~%"))
        (emit post-header)
        (let* ((file-stem
                (if (plusp (length filename))
                    (subseq filename
                            0
                            (or (position #\. filename :from-end t)
                                (length filename)))
                  "nil"))
               (entry-name (if dir-name
                               (format nil "* ~A: (~A)." dir-name
                                       (if (plusp (length file-stem)) file-stem dir-name))
                             (format nil "* (~A)." file-stem)))
               (description (if (and (plusp (length dir-desc))
                                     (char= (char dir-desc (1- (length dir-desc))) #\.))
                                dir-desc
                              (concatenate 'string dir-desc "."))))
          (emit
           (format nil "@dircategory ~A~%@direntry~%~23A ~A~%@end direntry~%~%"
                   (export-texinfo-escape-text context dir-category offset)
                   (export-texinfo-escape-text context entry-name offset)
                   (export-texinfo-escape-text context description offset))))
        (emit (format nil "@finalout~%@titlepage~%"))
        (when with-title
          (emit (format nil "@title ~A~%" (export-texinfo-escape-text
                                           context printed-title offset))))
        (when with-author
          (when author
            (emit (format nil "@author ~A~%" (export-texinfo-escape-text
                                              context author offset)))))
        (emit (format nil "@end titlepage~%~%"))
        (when toc (emit (format nil "@contents~%~%")))
        (emit (format nil "@ifnottex~%@node Top~%"))
        (emit (format nil "@top ~A~%" (export-texinfo-escape-text context title offset)))
        (when top-section
          (let* ((*export-current-output-offset* offset)
                 (frame (%make-export-render-frame :mode :block :output-offset offset))
                 (top-text (export-texinfo-render-children context top-section frame :block)))
            (when (plusp (length top-text))
              (emit (concatenate 'string (string #\Newline) top-text)))))
        (emit (format nil "@end ifnottex~%~%"))
        (progn
          (emit master-menu)
          (emit (string #\Newline)))
        (progn
          (emit body)
          (if (plusp (length body))
              (let* ((last-non-newline
                      (position-if-not
                       (lambda (character) (char= character #\Newline))
                       body :from-end t))
                     (trailing-newlines
                      (- (length body)
                         (if last-non-newline (1+ last-non-newline) 0))))
                (when (< trailing-newlines 2)
                  (emit (make-string (- 2 trailing-newlines)
                                     :initial-element #\Newline))))
            (emit (string #\Newline))))
        (emit "@bye")))))

(defun export-texinfo-node-transcoders ()
  (mapcar (lambda (type) (cons type #'export-texinfo-transcode-node))
          (node-types)))

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend :texinfo :retained-keywords '("TEXINFO") :renderer #'export-texinfo-render-body
   :template #'export-texinfo-render-template
   :make-state #'export-texinfo-make-state
   :transcoders (export-texinfo-node-transcoders)
   :accepted-keywords '("TEXINFO" "TEXINFO_FILENAME" "TEXINFO_CLASS"
                        "TEXINFO_HEADER" "TEXINFO_POST_HEADER"
                        "TEXINFO_DIR_CATEGORY" "TEXINFO_DIR_TITLE"
                        "TEXINFO_DIR_NAME" "TEXINFO_DIR_DESC"
                        "TEXINFO_PRINTED_TITLE" "SUBAUTHOR")
   :export-block-names '("texinfo")
   :export-snippet-names '("texinfo")))
(defun export-org-texinfo (document &rest arguments)
  "Export DOCUMENT with the :TEXINFO backend."
  (apply (function export-org) document :texinfo arguments))
