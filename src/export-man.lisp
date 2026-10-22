;;;; src/export-man.lisp
(in-package #:org-kit)

(defstruct (export-man-output (:constructor %make-export-man-output))
  buffer
  context
  (base-offset 0))

(defun export-man-make-output (context)
  (check-type *export-current-output-offset* (integer 0))
  (%make-export-man-output
   :buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
   :context context
   :base-offset *export-current-output-offset*))

(defun export-man-output-length (output)
  (fill-pointer (export-man-output-buffer output)))

(defun export-man-check-output (output added-length)
  (let* ((context (export-man-output-context output))
         (actual (+ (export-man-output-base-offset output)
                    (export-man-output-length output) added-length)))
    (export-check-output-actual actual (export-render-context-limits context)
                                (export-render-context-backend context)
                                *export-current-export-phase*)
    actual))

(defun export-man-write-string (output string)
  (when (plusp (length string))
    (export-man-check-output output (length string))
    (loop for character across string
          do (vector-push-extend character (export-man-output-buffer output))))
  output)

(defun export-man-write-char (output character)
  (export-man-check-output output 1)
  (vector-push-extend character (export-man-output-buffer output))
  output)

(defun export-man-output-string (output)
  (coerce (export-man-output-buffer output) 'string))

(defun export-man-append (context base-offset &rest strings)
  (let* ((*export-current-output-offset* base-offset)
         (output (export-man-make-output context)))
    (dolist (string strings)
      (when string (export-man-write-string output string)))
    (export-man-output-string output)))

(defun export-man-protect-text (text context base-offset)
  (let* ((*export-current-output-offset* base-offset)
         (output (export-man-make-output context)))
    (loop for character across text
          do (when (char= character #\-)
               (export-man-write-char output #\\))
             (export-man-write-char output character))
    (export-man-output-string output)))
(defun export-man-inline-value (node delimiter)
  (let* ((source (or (node-text node) (node-property node :value) ""))
         (end (position-if-not
               (lambda (character)
                 (member character (list #\Space #\Tab #\Newline #\Return)))
               source :from-end t)))
    (if (and end
             (> end 0)
             (char= (char source 0) delimiter)
             (char= (char source end) delimiter))
        (subseq source 1 end)
        source)))

(defun export-man-protect-example (text context base-offset)
  (let* ((*export-current-output-offset* base-offset)
         (output (export-man-make-output context)))
    (loop for character across text
          do (if (char= character #\\)
                 (export-man-write-string output "\\e")
                 (export-man-write-char output character)))
    (export-man-output-string output)))

(defun export-man-plain-text (text context base-offset)
  (let* ((*export-current-output-offset* base-offset)
         (output (export-man-make-output context))
         (language (getf (export-render-context-metadata context) :language))
         (quote-marks
           (and (export-option-value context :smart-quotes)
                (assoc (string-downcase (or language ""))
                            '(("ar" "«" "»" "‹" "›" "’")
                              ("da" "»" "«" "›" "‹" "’")
                              ("de" "„" "“" "‚" "‘" "’")
                              ("el" "«" "»" "“" "”" "’")
                              ("en" "“" "”" "‘" "’" "’")
                              ("es" "«" "»" "“" "”" "’")
                              ("fa" "«" "»" "‹" "›" "’")
                              ("fr" "« " " »" "“" "”" "’")
                              ("is" "„" "“" "‚" "‘" "’")
                              ("it" "“" "”" "‘" "’" "’")
                              ("nb" "«" "»" "‘" "’" "’")
                              ("nn" "«" "»" "‘" "’" "’")
                              ("no" "«" "»" "‘" "’" "’")
                              ("ro" "„" "”" "«" "»" "’")
                              ("ru" "«" "»" "„" "“" "’")
                              ("sl" "«" "»" "„" "“" "’")
                              ("sv" "”" "”" "’" "’" "’"))
                            :test #'string=)))
         (text
           (if quote-marks
               (let* ((replacements (make-array (length text)
                                                :initial-element nil))
                      (double-open nil)
                      (single-open nil)
                      (double-left (second quote-marks))
                      (double-right (third quote-marks))
                      (single-left (fourth quote-marks))
                      (single-right (fifth quote-marks))
                      (apostrophe (sixth quote-marks))
                      (single-quote (code-char 39)))
                 (loop for position below (length text)
                       for character = (char text position)
                       for previous = (and (plusp position)
                                           (char text (1- position)))
                       for next = (and (< (1+ position) (length text))
                                       (char text (1+ position)))
                       do (cond
                            ((char= character #\")
                             (if double-open
                                 (setf (aref replacements double-open) double-left
                                       (aref replacements position) double-right
                                       double-open nil)
                               (setf double-open position)))
                            ((char= character single-quote)
                             (cond
                               ((and previous next
                                     (alphanumericp previous)
                                     (alphanumericp next))
                                (setf (aref replacements position) apostrophe))
                               (single-open
                                (setf (aref replacements single-open) single-left
                                      (aref replacements position) single-right
                                      single-open nil))
                               ((and previous (alphanumericp previous))
                                (setf (aref replacements position) apostrophe))
                               (t
                                (setf single-open position))))))
                 (with-output-to-string (stream)
                   (loop for position below (length text)
                         for replacement = (aref replacements position)
                         do (if replacement
                                (write-string replacement stream)
                              (write-char (char text position) stream)))))
               text))
         (preserve-breaks-p (export-option-value context :preserve-breaks))
         (length (length text)))
    (labels ((write-protected-range (start end)
               (loop for position from start below end
                     for character = (char text position)
                     do (when (and (char= character #\\)
                                   (or (zerop position)
                                       (not (char= (char text (1- position)) #\\)))
                                   (or (= (1+ position) length)
                                       (not (find (char text (1+ position))
                                                  "%$#&{}~^_\\"))))
                          (export-man-write-char output #\$))
                        (export-man-write-char output character)))
             (write-preserved-line (start end break-p)
               (let ((content-end end))
                 (loop while (and (> content-end start)
                                  (find (char text (1- content-end))
                                        '(#\Space #\Tab)))
                       do (decf content-end))
                 (when (and (>= (- content-end start) 2)
                            (char= (char text (- content-end 1)) #\\)
                            (char= (char text (- content-end 2)) #\\))
                   (decf content-end 2))
                 (write-protected-range start content-end)
                 (when break-p
                   (export-man-write-string output (format nil ".br~%"))))))
      (if preserve-breaks-p
          (loop with start = 0
                for newline = (position #\Newline text :start start)
                for end = (or newline length)
                do (write-preserved-line start end (not (null newline)))
                   (if newline
                       (setf start (1+ newline))
                     (return)))
        (write-protected-range 0 length)))
    (export-man-output-string output)))

(defun export-man-render-child (context view offset &optional (mode :block))
  (let ((*export-current-output-offset* offset))
    (export-render-view context view :mode mode)))

(defun export-man-render-children (context view &optional (mode :block))
  (let* ((*export-current-output-offset* *export-current-output-offset*)
         (output (export-man-make-output context))
         (previous-block-p nil))
    (dolist (child (export-view-children view))
      (let* ((node (export-view-node child))
             (block-p (and (eq mode :block)
                           (member (node-type-category (node-type node)) '(:greater-element :lesser-element))))
             (rendered
               (export-man-render-child
                context child
                (+ (export-man-output-base-offset output)
                   (export-man-output-length output))
                mode))
             (output-length (export-man-output-length output)))
        (when (plusp (length rendered))
          (when (and block-p previous-block-p
                     (or (zerop output-length)
                         (not (char= (char (export-man-output-buffer output)
                                           (1- output-length))
                                     #\Newline))))
            (export-man-write-char output #\Newline))
          (export-man-write-string output rendered)
          (setf previous-block-p block-p))))
    (export-man-output-string output)))

(defun export-man-render-inline-node (context node offset)
  (let ((*export-current-output-offset* offset)
        (source-document (node-document node)))
    (labels ((make-view (current)
               (make-export-view
                current
                :children (mapcar (function make-view) (node-children current))
                :source-document source-document)))
      (export-render-view context (make-view node) :mode :inline))))

(defun export-man-render-title (context node view)
  (let* ((output (export-man-make-output context))
         (segments (node-property node :title))
         (macros (and view (getf (export-view-payload view) :title-macros))))
    (dolist (segment segments)
      (let* ((macro-entry (and (node-p segment)
                               (assoc segment macros :test (function eq))))
             (rendered
               (cond
                 ((stringp segment)
                  (export-man-plain-text
                   segment context (+ (export-man-output-base-offset output)
                                      (export-man-output-length output))))
                 (macro-entry
                  (export-man-render-children
                   context (make-export-view segment :children (cdr macro-entry))
                   :inline))
                 ((node-p segment)
                  (export-man-render-inline-node
                   context segment (+ (export-man-output-base-offset output)
                                      (export-man-output-length output))))
                 (t ""))))
        (export-man-write-string output rendered)))
    (export-man-output-string output)))

(defun export-man-minimum-indentation (text)
  (loop with start = 0
        with minimum = most-positive-fixnum
        for end = (or (position #\Newline text :start start) (length text))
        for first-content = (position-if-not
                             (lambda (character) (find character '(#\Space #\Tab)))
                             text :start start :end end)
        do (when first-content
             (setf minimum (min minimum (- first-content start))))
        if (= end (length text))
          return (if (= minimum most-positive-fixnum) 0 minimum)
        else do (setf start (1+ end))))

(defun export-man-remove-indentation (text context base-offset)
  (let* ((*export-current-output-offset* base-offset)
         (output (export-man-make-output context))
         (indent (export-man-minimum-indentation text))
         (start 0))
    (loop for end = (or (position #\Newline text :start start) (length text))
          for content-start = (min end (+ start indent))
          do (export-man-write-string output (subseq text content-start end))
             (when (< end (length text))
               (export-man-write-char output #\Newline))
             (if (= end (length text))
                 (return)
                 (setf start (1+ end))))
    (export-man-output-string output)))

(defun export-man-split-lines (text)
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (subseq text start end)
        while end
        do (setf start (1+ end))))

(defun export-man-render-headline (node view contents context)
  (let* ((level (or (node-property node :level) 1))
         (title (export-man-render-title context node view))
         (base-offset *export-current-output-offset*)
         (siblings
           (and (node-parent node)
                (remove-if-not
                 (lambda (sibling)
                   (and (eq (node-type sibling) :headline)
                        (eql (node-property sibling :level) level)))
                 (node-children (node-parent node)))))
         (first-sibling-p (eq node (first siblings)))
         (last-sibling-p (eq node (car (last siblings)))))
    (cond
      ((<= level 3)
       (export-man-append context base-offset
                          (if (= level 1) ".SH \"" ".SS \"")
                          title "\"" (string #\Newline) contents))
      (t
       (let ((body
               (export-man-append context base-offset
                                  (if first-sibling-p
                                      (format nil ".RS~%")
                                    "")
                                  ".TP" (string #\Newline)
                                  ".ft I" (string #\Newline)
                                  title (string #\Newline)
                                  ".ft" (string #\Newline)
                                  contents ".RE")))
         (if last-sibling-p
             (string-right-trim (list #\Space #\Tab #\Newline) body)
           body))))))

(defun export-man-keyword-value (document key)
  (let ((value nil))
    (map-nodes
     (lambda (node)
       (when (and (eq (node-type node) :keyword)
                  (string-equal (or (node-property node :key) "") key))
         (setf value (node-property node :value))))
     (document-root document))
    value))

(defun export-man-option-strings (text)
  (let ((position 0)
        (length (length (or text "")))
        (options nil))
    (labels ((separator-p (character)
               (find character (list #\Space #\Tab #\Newline #\,)))
             (skip-space ()
               (loop while (and (< position length)
                                (separator-p (char text position)))
                     do (incf position)))
             (read-token ()
               (let ((start position))
                 (loop while (and (< position length)
                                  (not (separator-p (char text position))))
                       do (incf position))
                 (subseq text start position)))
             (read-string-value ()
               (incf position)
               (let ((output (make-string-output-stream)))
                 (loop while (< position length)
                       for character = (char text position)
                       do (incf position)
                          (cond
                            ((char= character #\\)
                             (when (< position length)
                               (let ((escaped (char text position)))
                                 (incf position)
                                 (write-char
                                  (case escaped
                                    (#\n #\Newline)
                                    (#\t #\Tab)
                                    (otherwise escaped))
                                  output))))
                            ((char= character #\")
                             (return))
                            (t (write-char character output))))
                 (get-output-stream-string output))))
      (loop do
        (skip-space)
        (when (>= position length) (return))
        (let ((key (read-token)))
          (skip-space)
          (when (and (plusp (length key))
                     (char= (char key 0) #\:)
                     (< position length))
            (let ((value (if (char= (char text position) #\")
                             (read-string-value)
                             (read-token))))
              (push (cons (string-downcase key) value) options)))))
      (nreverse options))))

(defun export-man-template (context _preamble body _trailer)
  (declare (ignore _preamble _trailer))
  (let* ((metadata (export-render-context-metadata context))
         (title (getf metadata :title))
         (date (getf metadata :date))
         (title-enabled (if (export-option-explicit-p context :title)
                            (export-option-value context :title)
                            t))
         (date-enabled (if (export-option-explicit-p context :date)
                           (export-option-value context :date)
                           t))
         (title-text (if (and title-enabled (stringp title))
                         (export-man-plain-text title context
                                               *export-current-output-offset*)
                         " "))
         (date-text (if (and date-enabled (stringp date)) date ""))
         (class-options
           (export-man-option-strings
            (export-man-keyword-value
             (export-render-context-document context) "MAN_CLASS_OPTIONS")))
         (section (or (cdr (assoc ":section-id" class-options :test #'string=)) "1"))
         (release (cdr (assoc ":release" class-options :test #'string=)))
         (header (cdr (assoc ":header" class-options :test #'string=)))
         (output (export-man-make-output context)))
    (export-man-write-string output ".TH \"")
    (export-man-write-string output title-text)
    (export-man-write-string output "\" \"")
    (export-man-write-string output section)
    (export-man-write-string output "\" \"")
    (export-man-write-string output date-text)
    (export-man-write-string output "\" \"")
    (export-man-write-string output (or release ""))
    (export-man-write-char output #\")
    (when header
      (export-man-write-string output " \"")
      (export-man-write-string output header)
      (export-man-write-char output #\"))
    (export-man-write-char output #\Space)
    (export-man-write-char output #\Newline)
    (progn
      (export-man-write-string output body)
      (when (and (plusp (length body))
                 (not (char= (char body (1- (length body))) #\Newline)))
        (export-man-write-char output #\Newline)))
    (export-man-output-string output)))

(defun export-man-render-document (context)
  (let* ((output (export-man-make-output context))
         (previous-block-p nil))
    (dolist (view (export-render-context-root-views context))
      (let* ((node (export-view-node view))
             (block-p (member (node-type-category (node-type node)) '(:greater-element :lesser-element)))
             (rendered
               (export-man-render-child
                context view
                (+ (export-man-output-base-offset output)
                   (export-man-output-length output))))
             (output-length (export-man-output-length output)))
        (when (plusp (length rendered))
          (when (and block-p previous-block-p
                     (or (zerop output-length)
                         (not (char= (char (export-man-output-buffer output)
                                           (1- output-length))
                                     #\Newline))))
            (export-man-write-char output #\Newline))
          (export-man-write-string output rendered)
          (setf previous-block-p block-p))))
    (export-man-output-string output)))

(defun export-man-transcode (node view frame context)
  (let* ((type (node-type node))
         (offset *export-current-output-offset*)
         (contents
          (if (member type '(:code :entity :example-block :export-block
                                   :export-snippet :fixed-width :inline-src-block
                                   :keyword :line-break :statistics-cookie :src-block
                                   :table :table-row :target :timestamp :verbatim
                                   :node-property)
                      :test #'eq)
              ""
            (export-man-render-children
             context view
             (if (or (member type '(:paragraph :bold :italic :strike-through
                                               :underline :subscript :superscript
                                               :radio-target :table-cell)
                             :test #'eq)
                     (and (member type '(:macro :section) :test #'eq)
                          (eq (%export-render-frame-mode frame) :inline))) :inline :block)))))
    (case type
      (:bold (export-man-append context offset "\\fB" contents "\\fP"))
      (:italic (export-man-append context offset "\\fI" contents "\\fP"))
      (:strike-through (export-man-append context offset "\\fI" contents "\\fP"))
      (:underline (export-man-append context offset "\\fI" contents "\\fP"))
      (:code
       (export-man-append context offset "\\fC"
                          (export-man-protect-text
                           (export-man-inline-value node #\~) context offset)
                          "\\fP"))
      (:verbatim
       (export-man-append context offset "\\fI"
                          (export-man-protect-text
                           (export-man-inline-value node #\=) context offset)
                          "\\fP"))
      (:center-block
       (let* ((lines (export-man-split-lines contents))
              (label (node-property node :name)))
         (export-man-append context offset
                            (if label (format nil "~A~%.br~%" label) "")
                            (format nil ".ce ~D~%.nf~%" (max 0 (1- (length lines))))
                            contents (format nil "~%.fi"))))
      (:drawer contents)
      (:dynamic-block contents)
      (:entity (or (node-property node :utf-8)
                   (node-property node :utf8) ""))
      (:example-block
       (export-man-append context offset (format nil ".RS~%.nf~%")
                          (export-man-protect-example
                           (export-man-remove-indentation
                            (or (node-property node :value) "") context offset)
                           context offset)
                          (format nil "~%.fi~%.RE")))
      (:export-block
       (if (string-equal (or (node-property node :type) "") "MAN")
           (export-man-remove-indentation
            (or (node-property node :value) "") context offset)
         ""))
      (:export-snippet
       (if (string-equal (or (node-property node :back-end) "") "man")
           (or (node-property node :value) "") ""))
      (:fixed-width
       (export-man-append context offset (format nil "\\fC~%")
                          (export-man-remove-indentation
                           (or (node-property node :value) "") context offset)
                          (format nil "~%\\fP~%")))
      (:headline (export-man-render-headline node view contents context))
      (:inline-src-block
       (let* ((header-args (babel-parse-header-arguments
                            (node-property node :parameters)))
              (exports (cdr (assoc "EXPORTS" header-args :test (function string-equal)))))
         (if (and exports
                  (or (string-equal exports "code")
                      (string-equal exports "both")))
             (export-man-append context offset
                                (format nil ".RS~%.nf~%\\fC~%")
                                (export-man-protect-example
                                 (or (node-property node :value) "")
                                 context offset)
                                (format nil "~%\\fP~%.fi~%.RE~%"))
           "")))
      (:item (export-man-transcode-item node contents context offset))
      (:keyword
       (if (string-equal (or (node-property node :key) "") "MAN")
           (export-man-append context offset
                              (or (node-property node :value) "")
                              (string #\Newline))
         ""))
      (:line-break (format nil "~%.br~%"))
      (:link (export-man-transcode-link node contents context offset))
      (:paragraph (if (and (eq (%export-render-frame-mode frame) :block) (node-parent node) (eq (node-type (node-parent node)) :section)) (export-man-append context offset (format nil ".PP~%") contents) contents))
      ((:plain-list :footnote-reference :footnote-definition) (if (eq type :plain-list) contents ""))
      ((:plain-text :text) (let* ((document (export-view-source-document view)) (begin (node-begin node)) (end (node-end node)) (text (and document begin end (subseq (document-text document) begin end)))) (export-man-plain-text (or text (node-text node) "") context offset)))
      (:property-drawer
       (if (export-option-value context :properties)
           (let* ((source-document (export-view-source-document view))
                  (source-begin (node-begin node))
                  (source-end (node-end node))
                  (source-node
                    (when (and source-document source-begin source-end)
                      (let ((match nil))
                        (map-nodes
                         (lambda (candidate)
                           (when (and (null match)
                                      (eq (node-type candidate) :property-drawer)
                                      (eql (node-begin candidate) source-begin)
                                      (eql (node-end candidate) source-end))
                             (setf match candidate)))
                         (document-root source-document))
                        match)))
                  (*export-current-output-offset* offset)
                  (property-output (export-man-make-output context)))
             (dolist (property (node-children (or source-node node)))
               (when (eq (node-type property) :node-property)
                 (when (plusp (export-man-output-length property-output))
                   (export-man-write-char property-output #\Newline))
                 (export-man-write-string
                  property-output (or (node-property property :key) ""))
                 (export-man-write-char property-output #\Colon)
                 (let ((value (node-property property :value)))
                   (when value
                     (export-man-write-char property-output #\Space)
                     (export-man-write-string property-output value)))
                 (export-man-write-char property-output #\Newline)))
             (let ((property-contents
                     (export-man-output-string property-output)))
               (if (plusp (length property-contents))
                   (export-man-append context offset
                                      (format nil ".RS~%.nf~%")
                                      property-contents
                                      (format nil "~%.fi~%.RE~%"))
                 "")))
         ""))
      (:quote-block
       (export-man-append context offset (format nil ".RS~%") contents
                          (format nil "~%.RE")))
      (:radio-target contents)
      (:section contents)
      (:special-block contents)
      (:src-block
       (let* ((mode (getf (export-view-payload view) :exports))
              (results (export-man-render-children context view :block))
              (code
               (export-man-append
                context offset (format nil ".RS~%.nf~%\\fC")
                (export-man-protect-example
                 (export-man-remove-indentation
                  (or (node-property node :value) "") context offset)
                 context offset)
                (format nil "\\fP~%.fi~%.RE~%"))))
         (case mode
           (:none "")
           (:results results)
           (:both
            (if (zerop (length results))
                code
              (export-man-append context offset code results)))
           (otherwise code))))
      (:statistics-cookie (or (node-property node :value) ""))
      (:subscript (export-man-append context offset "\\d\\s-2" contents "\\s+2\\u"))
      (:superscript (export-man-append context offset "\\u\\s-2" contents "\\s+2\\d"))
      (:table (export-man-transcode-table view context))
      (:table-cell
       (export-man-append context offset contents
                          (if (export-man-next-sibling-p node) (string #\Tab) "")))
      (:table-row
       (if (eq (node-property node :type) :rule)
           ""
         (export-man-append context offset contents
                            (if (export-man-row-below-rule-p node)
                                (format nil "~%_") ""))))
      (:node-property
       (export-man-append context offset
                          (or (node-property node :key) "") ":"
                          (let ((value (node-property node :value)))
                            (if value (concatenate 'string " " value) ""))))
      (:target
       (let* ((number (mod (node-begin node) #x10000000))
              (digits (make-string 7 :initial-element #\0)))
         (loop for position downfrom 6 to 0
               do (setf (char digits position)
                        (char "0123456789abcdef" (mod number 16))
                        number (floor number 16)))
         (export-man-append context offset "\\fIorg" digits "\\fP")))
      (:timestamp "")
      (:verse-block (export-man-append context offset
                                       (format nil ".RS~%.ft I~%")
                                       contents (format nil "~%.ft~%.RE")))
      (otherwise contents))))

(defun export-man-next-sibling-p (node)
  (let* ((parent (node-parent node))
         (children (and parent (node-children parent))))
    (and children
         (not (eq node (car (last children)))))))

(defun export-man-row-below-rule-p (node)
  (let* ((parent (node-parent node))
         (siblings (and parent (node-children parent)))
         (tail (member node siblings :test #'eq)))
    (and tail (cdr tail)
         (eq (node-property (cadr tail) :type) :rule))))

(defun export-man-transcode-item (node contents context offset)
  (let* ((bullet (string-trim (list #\Space #\Tab)
                              (or (node-property node :bullet) "-")))
         (parent (node-parent node))
         (list-type (and parent (node-property parent :type)))
         (tag (node-property node :tag))
         (marker (cond ((string= bullet "-") "\\(em")
                       ((string= bullet "*") "\\(bu")
                       ((eq list-type :ordered) (format nil "~A " bullet))
                       (t "\\(dg"))))
    (if tag
        (let ((tag-output (export-man-make-output context)))
          (dolist (segment tag)
            (let* ((segment-offset (+ (export-man-output-base-offset tag-output)
                                      (export-man-output-length tag-output)))
                   (rendered
                     (cond
                       ((stringp segment)
                        (export-man-plain-text segment context segment-offset))
                       ((node-p segment)
                        (export-man-render-inline-node context segment segment-offset))
                       (t ""))))
              (export-man-write-string tag-output rendered)))
          (export-man-append context offset ".TP" (string #\Newline)
                             "\\fB" (export-man-output-string tag-output)
                             "\\fP" (string #\Newline)
                             (string-trim (list #\Space #\Tab #\Newline)
                                          contents)
                             (string #\Newline)))
      (export-man-append context offset ".IP " marker (format nil " 4~%")
                         (string-trim (list #\Space #\Tab #\Newline) contents)
                         (string #\Newline)))))

(defun export-man-transcode-link (node description context offset)
  (let* ((type (node-property node :type))
         (path (node-property node :path))
         (destination
           (and type path
                (if (string= type "file")
                    (cond
                      ((and (>= (length path) 2)
                            (char= (char path 0) #\/)
                            (char= (char path 1) #\/))
                       (concatenate (quote string) "file:" path))
                      ((and (plusp (length path))
                            (char= (char path 0) #\/))
                       (concatenate (quote string) "file://" path))
                      (t path))
                  (concatenate (quote string) type ":" path)))))
    (cond
      ((and destination (plusp (length description)))
       (export-man-append context offset destination " \\fBat\\fP \\fI"
                          description "\\fP"))
      (destination (export-man-append context offset "\\fI" destination "\\fP"))
      (t (export-man-append context offset "\\fI" description "\\fP")))))

(defun export-man-table-row-views (view)
  (export-view-children view))

(defun export-man-transcode-table (view context)
  (let* ((rows (export-man-table-row-views view))
         (standard-rows (remove-if
                         (lambda (row)
                           (eq (node-property (export-view-node row) :type) :rule))
                         rows))
         (first-row (first standard-rows))
         (cells (and first-row (export-view-children first-row)))
         (column-count (length cells))
         (output (export-man-make-output context))
         (emitted 0))
    (export-man-write-string output (format nil ".TS~% center,box;~%~%"))
    (loop for index below column-count
          do (when (plusp index) (export-man-write-char output #\Space))
             (export-man-write-char output #\l))
    (export-man-write-string output (format nil " .~%"))
    (dolist (row rows)
      (let ((row-node (export-view-node row)))
        (cond
          ((eq (node-property row-node :type) :rule)
           nil)
          (t
           (when (plusp emitted)
             (export-man-write-char output #\Newline))
           (export-man-write-string
            output (export-man-render-children context row :inline))
           (when (export-man-row-below-rule-p row-node)
             (export-man-write-string output (format nil "~%_")))
           (incf emitted)))))
    (export-man-write-string output (format nil "~%.TE~%.TB \"\""))
    (export-man-output-string output)))

(defun export-man-clone-node-and-view (view root-document)
  (labels ((clone-view (view)
             (let* ((source-node (export-view-node view))
                    (source-document (or (export-view-source-document view)
                                         (node-document source-node)
                                         root-document))
                    (children (mapcar #'clone-view (export-view-children view)))
                    (node (make-node
                           (node-type source-node)
                           :begin (node-begin source-node)
                           :end (node-end source-node)
                           :contents-begin (node-contents-begin source-node)
                           :contents-end (node-contents-end source-node)
                           :post-affiliated (node-post-affiliated source-node)
                           :post-blank
                           (if (member (node-type-category (node-type source-node))
                                       '(:greater-element :lesser-element))
                               0
                               (node-post-blank source-node))
                           :properties (node-properties source-node)
                           :children (mapcar #'export-view-node children))))
               (%make-export-view :node node :children children
                                  :payload (export-view-payload view)
                                  :source-document source-document))))
    (let ((cloned-view (clone-view view)))
      (setf (%node-document (export-view-node cloned-view))
            (export-view-source-document cloned-view))
      cloned-view)))

(defun export-man-remove-post-blank (views context)
  (let ((document (export-render-context-document context)))
    (mapcar (lambda (view)
              (export-man-clone-node-and-view view document))
            views)))

(eval-when (:load-toplevel :execute)
  (%register-builtin-export-backend :man
   :renderer #'export-man-render-document
   :template #'export-man-template
   :transcoders (mapcar (lambda (type) (cons type #'export-man-transcode))
                        '(:bold :center-block :code :drawer :dynamic-block :entity
                          :example-block :export-block :export-snippet :fixed-width
                          :footnote-definition :footnote-reference
                          :headline :inline-src-block :italic :item :keyword
                          :line-break :link :node-property :paragraph :plain-list :plain-text
                          :property-drawer :quote-block :radio-target :section
                          :special-block :src-block :statistics-cookie :strike-through
                          :subscript :superscript :table :table-cell :table-row
                          :target :timestamp :underline :verbatim :verse-block))
   :accepted-keywords '("MAN" "MAN_CLASS" "MAN_CLASS_OPTIONS" "MAN_HEADER"
                        "INDEX" "TOC")
   :retained-keywords '("MAN")
   :export-block-names '("man")
   :export-snippet-names '("man")
   :filters (list (cons :parse-tree (list #'export-man-remove-post-blank)))))
(defun export-org-man (document &rest arguments)
  "Export DOCUMENT with the man backend."
  (apply (function export-org) document :man arguments))
