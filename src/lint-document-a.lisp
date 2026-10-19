(in-package #:org-kit)

(defun %lint-a-whitespace-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return)))

(defun %lint-a-words (string)
  (let ((start nil)
        (words nil))
    (loop for position from 0 to (length string)
          for separator = (or (= position (length string))
                              (%lint-a-whitespace-p (char string position)))
          do (if separator
                 (when start
                   (push (subseq string start position) words)
                   (setf start nil))
                 (unless start (setf start position))))
    (nreverse words)))

(defun %lint-a-next-sibling (node)
  (let ((siblings (and (node-parent node)
                       (node-children (node-parent node)))))
    (loop for (current next) on siblings
          when (eq current node)
            return next)))

(defun %lint-a-previous-siblings (node)
  (let ((siblings (and (node-parent node)
                       (node-children (node-parent node)))))
    (loop for current in siblings
          until (eq current node)
          collect current)))

(defun %lint-a-under-p (node ancestor)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        thereis (eq parent ancestor)))

(defun %lint-a-line-bounds (text start)
  (let ((newline (position #\Newline text :start start)))
    (if newline
        (values (if (and (> newline start)
                         (char= (char text (1- newline)) #\Return))
                    (1- newline)
                    newline)
                (1+ newline))
        (values (length text) (length text)))))

(defun %lint-a-offset-p (document offset)
  (and (integerp offset) (<= 0 offset (length (document-text document)))))

(defun %lint-a-line-keyword (line)
  (let* ((start (position-if-not (lambda (character)
                                   (find character '(#\Space #\Tab)))
                                 line))
         (colon (and start
                     (<= (+ start 2) (length line))
                     (string= "#" line :start2 start :end2 (1+ start))
                     (string= "+" line :start2 (1+ start) :end2 (+ start 2))
                     (position #\: line :start (+ start 2)))))
    (when (and start colon)
      (values (string-upcase
               (string-trim '(#\Space #\Tab)
                            (subseq line (+ start 2) colon)))
              (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))))

(defun %lint-a-name-keyword-offset (document node name)
  (let* ((text (document-text document))
         (position (or (node-begin node) 0))
         (limit (or (node-post-affiliated node) (node-begin node) 0)))
    (loop while (< position limit)
          do (multiple-value-bind (line-end next)
                 (%lint-a-line-bounds text position)
               (multiple-value-bind (key value)
                   (%lint-a-line-keyword (subseq text position line-end))
                 (when (and key (string= key "NAME") (string= value name))
                   (return-from %lint-a-name-keyword-offset position)))
               (setf position (max next (1+ position))))
    (node-begin node))))

(defun %lint-a-duplicate-reports (document nodes key-function position-function
                                 checker message-function)
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (node nodes)
      (let ((key (funcall key-function node)))
        (when key (push node (gethash key groups)))))
    (let ((reports nil))
      (maphash
       (lambda (key reversed-nodes)
         (when (cdr reversed-nodes)
           (dolist (node reversed-nodes)
             (push (%lint-report document checker
                                 (funcall position-function node key)
                                 (funcall message-function key))
                   reports))))
       groups)
      (remove nil reports))))

(defun %lint-a-obsolete-affiliated-name (key)
  (cond ((string= key "HEADERS") "HEADER")
        ((string= key "RESULT") "RESULTS")
        (t "NAME")))

(defun %lint-misplaced-heading (document context)
  (declare (ignore context))
  (let* ((text (document-text document))
         (length (length text))
         (containers (remove-if-not
                      (lambda (node)
                        (member (node-type node) (quote (:paragraph :headline))))
                      (%lint-nodes document)))
         (reports nil))
    (loop for position from 0 below length
          for character = (char text position)
          when (not (find character (quote (#\* #\Newline #\Return #\Space #\,))))
            do (let ((star-position (1+ position)))
                 (loop while (and (< star-position length)
                                  (char= (char text star-position) #\*))
                       do (incf star-position))
                 (when (and (>= (- star-position (1+ position)) 2)
                            (< star-position length)
                            (char= (char text star-position) #\Space)
                            (find-if (lambda (node)
                                       (and (node-begin node) (node-end node)
                                            (<= (node-begin node) position)
                                            (< position (node-end node))))
                                     containers))
                   (push (%lint-report document "misplaced-heading" position
                                       "Possibly misplaced heading line")
                         reports))))
    (remove nil reports)))

(defun %lint-duplicate-custom-id (document context)
  (declare (ignore context))
  (%lint-a-duplicate-reports
   document
   (remove-if-not
    (lambda (node)
      (and (eq (node-type node) :node-property)
           (string-equal (or (node-property node :key) "") "CUSTOM_ID")))
    (%lint-nodes document))
   (lambda (node) (node-property node :value))
   (lambda (node key) (declare (ignore key)) (node-begin node))
   "duplicate-custom-id"
   (lambda (key) (format nil "Duplicate CUSTOM_ID property \"~A\"" key))))

(defun %lint-duplicate-name (document context)
  (declare (ignore context))
  (%lint-a-duplicate-reports
   document
   (remove-if-not (lambda (node) (node-property node :name))
                  (%lint-nodes document))
   (lambda (node) (node-property node :name))
   (lambda (node key) (%lint-a-name-keyword-offset document node key))
   "duplicate-name"
   (lambda (key) (format nil "Duplicate NAME \"~A\"" key))))

(defun %lint-duplicate-target (document context)
  (declare (ignore context))
  (%lint-a-duplicate-reports
   document (%lint-nodes document :target)
   (lambda (node) (%lint-a-words (or (node-property node :value) "")))
   (lambda (node key) (declare (ignore key)) (node-begin node))
   "duplicate-target"
   (lambda (key) (format nil "Duplicate target <<~A>>"
                         (format nil "~{~A~^ ~}" key)))))

(defun %lint-duplicate-footnote-definition (document context)
  (declare (ignore context))
  (%lint-a-duplicate-reports
   document (%lint-nodes document :footnote-definition)
   (lambda (node) (node-property node :label))
   (lambda (node key) (declare (ignore key)) (node-post-affiliated node))
   "duplicate-footnote-definition"
   (lambda (key) (format nil "Duplicate footnote definition \"~A\"" key))))

(defun %lint-affiliated-keyword-p (key)
  (or (member key '("NAME" "CAPTION" "HEADER" "PLOT" "RESULT" "RESULTS"
                    "DATA" "LABEL" "RESNAME" "SOURCE" "SRCNAME" "TBLNAME"
                    "HEADERS")
              :test #'string=)
      (and (>= (length key) 5)
           (string= key "ATTR_" :end1 5 :end2 5)
           (> (length key) 5)
           (loop for character across (subseq key 5)
                 always (or (alphanumericp character)
                           (find character "_-"))))))

(defun %lint-orphaned-affiliated-keywords (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document :keyword)
        for key = (node-property node :key)
        when (and (stringp key)
                  (%lint-affiliated-keyword-p key)
                  (not (member key '("RESULT" "RESULTS") :test #'string=))
                  (%lint-a-offset-p document (node-post-affiliated node)))
          collect (%lint-report document "orphaned-affiliated-keywords"
                                (node-post-affiliated node)
                                (format nil "Orphaned affiliated keyword: \"~A\""
                                        key))))

(defun %lint-combining-keywords-with-affiliated (document context)
  (declare (ignore context))
  (loop for keyword in (%lint-nodes document :keyword)
        for next = (%lint-a-next-sibling keyword)
        when (and next (= (node-post-blank keyword) 0)
                  (%lint-a-offset-p document (node-begin keyword))
                  (node-begin next) (node-post-affiliated next)
                  (< (node-begin next) (node-post-affiliated next)))
          collect (%lint-report
                   document "combining-keywords-with-affiliated"
                   (node-begin keyword)
                   (format nil "Independent keyword ~A may be confused with affiliated keywords below"
                           (node-property keyword :key)))))

(defun %lint-obsolete-affiliated-keywords (document context)
  (declare (ignore context))
  (loop for keyword in (%lint-nodes document :keyword)
        for key = (node-property keyword :key)
        for next = (%lint-a-next-sibling keyword)
        when (and (member key '("DATA" "LABEL" "RESNAME" "SOURCE" "SRCNAME"
                                "TBLNAME" "RESULT" "HEADERS")
                          :test #'string=)
                  next (= (node-post-blank keyword) 0)
                  (= (node-end keyword) (node-begin next))
                  (%lint-a-offset-p document (node-begin keyword)))
          collect (%lint-report
                   document "obsolete-affiliated-keywords"
                   (node-begin keyword)
                   (format nil "Obsolete affiliated keyword: \"~A\".  Use \"~A\" instead"
                           key (%lint-a-obsolete-affiliated-name key)))))

(defun %lint-deprecated-export-blocks (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document :special-block)
        for type = (node-property node :type)
        when (and (stringp type)
                  (member (string-upcase type)
                          '("ASCII" "BEAMER" "HTML" "LATEX" "MAN" "MARKDOWN"
                            "MD" "ODT" "ORG" "TEXINFO")
                          :test #'string=)
                  (%lint-a-offset-p document (node-post-affiliated node)))
          collect (%lint-report
                   document "deprecated-export-blocks"
                   (node-post-affiliated node)
                   (format nil "Deprecated syntax for export block.  Use \"BEGIN_EXPORT ~A\" instead"
                           (string-upcase type)))))

(defun %lint-missing-language-in-src-block (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document :src-block)
        for language = (node-property node :language)
        unless (or (and (stringp language) (plusp (length language)))
                   (not (%lint-a-offset-p document (node-post-affiliated node))))
          collect (%lint-report document "missing-language-in-src-block"
                                (node-post-affiliated node)
                                "Missing language in source block")))

(defun %lint-missing-backend-in-export-block (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document :export-block)
        for type = (node-property node :type)
        when (and (or (null type) (and (stringp type) (zerop (length type))))
                  (%lint-a-offset-p document (node-post-affiliated node)))
          collect (%lint-report document "missing-backend-in-export-block"
                                (node-post-affiliated node)
                                "Missing backend in export block")))

(defun %lint-outer-bracketed-p (string)
  (and (stringp string) (>= (length string) 2)
       (char= (char string 0) #\[)
       (char= (char string (1- (length string))) #\])))

(defun %lint-invalid-babel-call-block (document context)
  (declare (ignore context))
  (loop for node in (%lint-nodes document :babel-call)
        for call = (node-property node :call)
        for end-header = (node-property node :end-header)
        when (and (%lint-a-offset-p document (node-post-affiliated node))
                  (or (null call) (and (stringp call) (zerop (length call)))))
          collect (%lint-report document "invalid-babel-call-block"
                                (node-post-affiliated node)
                                "Invalid syntax in babel call block")
        else when (and (%lint-a-offset-p document (node-post-affiliated node))
                       (%lint-outer-bracketed-p end-header))
          collect (%lint-report document "invalid-babel-call-block"
                                (node-post-affiliated node)
                                "Babel call's end header must not be wrapped within brackets")))

(defun %lint-deprecated-category-setup (document context)
  (declare (ignore context))
  (let ((seen nil)
        (reports nil))
    (dolist (node (%lint-nodes document :keyword))
      (when (and (string= (or (node-property node :key) "") "CATEGORY")
                 (%lint-a-offset-p document (node-post-affiliated node)))
        (when seen
          (push (%lint-report
                 document "deprecated-category-setup"
                 (node-post-affiliated node)
                 "Spurious CATEGORY keyword.  Set :CATEGORY: property instead")
                reports))
        (setf seen t)))
    (nreverse reports)))

(defun %lint-a-coderef-line-p (line reference)
  (let* ((end (position-if-not (lambda (character)
                                 (find character '(#\Space #\Tab)))
                               line :from-end t))
         (trimmed (if end (subseq line 0 (1+ end)) ""))
         (marker (format nil "(ref:~A)" reference)))
    (and (>= (length trimmed) (length marker))
         (string= marker trimmed :start2 (- (length trimmed) (length marker))))))

(defun %lint-a-coderef-in-block-p (node reference)
  (let ((value (node-property node :value)))
    (when (stringp value)
      (loop with position = 0
            while (< position (length value))
            for (line-end next) = (multiple-value-list
                                   (%lint-a-line-bounds value position))
            thereis (%lint-a-coderef-line-p (subseq value position line-end)
                                            reference)
            do (setf position next)))))

(defun %lint-invalid-coderef-link (document context)
  (declare (ignore context))
  (let ((blocks (append (%lint-nodes document :src-block)
                        (%lint-nodes document :example-block))))
    (loop for link in (%lint-nodes document :link)
          for reference = (node-property link :path)
          when (and (%lint-a-offset-p document (node-begin link))
                    (string= (or (node-property link :type) "") "coderef")
                    (not (some (lambda (block)
                                 (%lint-a-coderef-in-block-p block reference))
                               blocks)))
            collect (%lint-report document "invalid-coderef-link"
                                  (node-begin link)
                                  (format nil "Unknown coderef \"~A\"" reference)))))

(defun %lint-a-custom-id-present-p (document id)
  (some (lambda (node)
          (and (eq (node-type node) :node-property)
               (string= (or (node-property node :key) "") "CUSTOM_ID")
               (string= (or (node-property node :value) "") id)))
        (%lint-nodes document)))

(defun %lint-invalid-custom-id-link (document context)
  (declare (ignore context))
  (loop for link in (%lint-nodes document :link)
        for path = (node-property link :path)
          when (and (%lint-a-offset-p document (node-begin link))
                    (string= (or (node-property link :type) "") "custom-id")
                  (not (%lint-a-custom-id-present-p document path)))
          collect (%lint-report document "invalid-custom-id-link"
                                (node-begin link)
                                (format nil "Unknown custom ID \"~A\"" path))))

(defun %lint-a-uri-scheme-p (path)
  (let ((colon (and (stringp path) (position #\: path))))
    (and colon (> colon 0)
         (alpha-char-p (char path 0))
         (loop for character across (subseq path 1 colon)
               always (or (alphanumericp character)
                          (find character "+.-"))))))

(defun %lint-a-search-equal-p (left right)
  (and (stringp left) (stringp right)
       (equal (mapcar #'string-upcase (%lint-a-words left))
              (mapcar #'string-upcase (%lint-a-words right)))))

(defun %lint-a-fuzzy-target-present-p (document path)
  (let* ((heading-only (and (plusp (length path))
                            (char= (char path 0) #\*)))
         (search (if heading-only (subseq path 1) path)))
    (some
     (lambda (node)
       (case (node-type node)
         (:headline
          (or (%lint-a-search-equal-p search
                                      (node-property node :raw-value))
              (and (not heading-only)
                   (%lint-a-custom-id-present-p document search))))
         (:target (and (not heading-only)
                       (%lint-a-search-equal-p
                        search (node-property node :value))))
         (otherwise
          (and (not heading-only)
               (or (and (node-property node :name)
                        (%lint-a-search-equal-p
                         search (node-property node :name)))
                   (and (node-property node :results)
                        (%lint-a-search-equal-p
                         search (node-property node :results))))))))
     (%lint-nodes document))))

(defun %lint-invalid-fuzzy-link (document context)
  (declare (ignore context))
  (loop for link in (%lint-nodes document :link)
        for path = (node-property link :path)
        when (and (%lint-a-offset-p document (node-begin link))
                  (string= (or (node-property link :type) "") "fuzzy")
                  (not (%lint-a-uri-scheme-p path))
                  (not (%lint-a-fuzzy-target-present-p document path)))
          collect (%lint-report
                   document "invalid-fuzzy-link" (node-begin link)
                   (format nil "Unknown fuzzy location \"~A\""
                           (if (and (plusp (length path))
                                    (char= (char path 0) #\*))
                               (subseq path 1)
                               path)))))

(defun %lint-trailing-bracket-after-link (document context)
  (declare (ignore context))
  (let ((text (document-text document)))
    (loop for link in (%lint-nodes document :link)
          for end = (node-end link)
          when (and (%lint-a-offset-p document (node-begin link))
                    end (< end (length text))
                    (char= (char text end) #\]))
            collect (%lint-report document "trailing-bracket-after-link"
                                  (node-begin link)
                                  "Trailing ']' after link end"))))

(defun %lint-unclosed-brackets-in-link-description (document context)
  (declare (ignore context))
  (let ((text (document-text document)))
    (loop for link in (%lint-nodes document :link)
          for start = (node-contents-begin link)
          for end = (node-contents-end link)
          when (and (%lint-a-offset-p document (node-begin link))
                    start end (<= start end))
            append (let ((balance 0))
                     (loop for position from start below end
                           for character = (char text position)
                           do (case character
                                (#\[ (incf balance))
                                (#\] (decf balance))))
                     (when (plusp balance)
                       (list
                        (%lint-report
                         document "unclosed-brackets-in-link-description"
                         (node-begin link)
                         (format nil
                                 "No closing ']' matches '[' in link description: ~A"
                                 (subseq text start end)))))))))

(defun %lint-special-property-in-properties-drawer (document context)
  (declare (ignore context))
  (let ((special-properties
          '("ALLTAGS" "BLOCKED" "CLOCKSUM" "CLOCKSUM_T" "CLOSED" "DEADLINE"
            "FILE" "ITEM" "PRIORITY" "SCHEDULED" "TAGS" "TIMESTAMP"
            "TIMESTAMP_IA" "TODO")))
    (loop for property in (%lint-nodes document :node-property)
          for key = (node-property property :key)
          when (and (stringp key)
                    (member key special-properties :test #'string-equal)
                    (some (lambda (drawer)
                            (and (eq (node-type drawer) :property-drawer)
                                 (%lint-a-under-p property drawer)))
                          (%lint-nodes document))
                    (%lint-a-offset-p document (node-begin property)))
            collect (%lint-report
                     document "special-property-in-properties-drawer"
                     (node-begin property)
                     (format nil "Special property \"~A\" found in a properties drawer"
                             key)))))

(defun %lint-a-headline-ancestor-p (node)
  (loop for parent = (node-parent node) then (node-parent parent)
        while parent
        thereis (eq (node-type parent) :headline)))

(defun %lint-a-in-footnote-excluded-subtree-p (node heading)
  (loop for parent = (node-parent node) then (node-parent parent)
        while (and parent (not (eq parent heading)))
        thereis (member (node-type parent)
                        '(:footnote-definition :property-drawer)
                        :test #'eq)))

(defun %lint-obsolete-properties-drawer (document context)
  (declare (ignore context))
  (loop for drawer in (%lint-nodes document :drawer)
        when (and (string= (or (node-property drawer :drawer-name) "") "PROPERTIES")
                  (%lint-a-offset-p document (node-post-affiliated drawer)))
          collect (let* ((previous (%lint-a-previous-siblings drawer))
                         (before (mapcar #'node-type previous))
                         (headline-p (%lint-a-headline-ancestor-p drawer))
                         (correct-contents-p
                           (if headline-p
                               (member before '(nil (:planning)) :test #'equal)
                               (member before '(nil (:comment)) :test #'equal)))
                         (message (if correct-contents-p
                                      "Incorrect contents for PROPERTIES drawer"
                                      "Incorrect location for PROPERTIES drawer")))
                    (%lint-report document "obsolete-properties-drawer"
                                  (node-post-affiliated drawer) message))))

(defun %lint-a-duration-unit-p (unit)
  (member (string-downcase unit)
          '("y" "w" "d" "h" "m" "min") :test #'string=))

(defun %lint-a-duration-components-p (string)
  (let ((position 0)
        (length (length string))
        (found nil))
    (loop
      do (loop while (and (< position length)
                          (find (char string position) '(#\Space #\Tab)))
               do (incf position))
         (when (= position length) (return))
         (let ((number-start position))
           (loop while (and (< position length)
                            (digit-char-p (char string position)))
                 do (incf position))
           (unless (> position number-start) (return-from %lint-a-duration-components-p nil))
           (when (and (< position length) (char= (char string position) #\.))
             (incf position)
             (loop while (and (< position length)
                              (digit-char-p (char string position)))
                   do (incf position)))
           (loop while (and (< position length)
                            (find (char string position) '(#\Space #\Tab)))
                 do (incf position))
           (let ((unit-start position))
             (loop while (and (< position length)
                              (alpha-char-p (char string position)))
                   do (incf position))
             (unless (%lint-a-duration-unit-p (subseq string unit-start position))
               (return-from %lint-a-duration-components-p nil)))
           (setf found t)))
    found))

(defun %lint-a-clock-duration-p (string)
  (let* ((string (string-trim '(#\Space #\Tab) string))
         (first-colon (position #\: string))
         (second-colon (and first-colon
                            (position #\: string :start (1+ first-colon)))))
    (and first-colon
         (or (null second-colon)
             (null (position #\: string :start (1+ second-colon))))
         (let ((hours (subseq string 0 first-colon))
               (minutes (subseq string (1+ first-colon)
                                (or second-colon (length string))))
               (seconds (and second-colon
                             (subseq string (1+ second-colon)))))
           (and (plusp (length hours))
                (every #'digit-char-p hours)
                (= (length minutes) 2)
                (every #'digit-char-p minutes)
                (or (null seconds)
                    (and (= (length seconds) 2)
                         (every #'digit-char-p seconds))))))))

(defun %lint-a-mixed-duration-p (string)
  (loop for split from 1 below (length string)
        when (and (find (char string (1- split)) '(#\Space #\Tab))
                  (%lint-a-clock-duration-p (subseq string split))
                  (%lint-a-duration-components-p
                   (string-trim '(#\Space #\Tab) (subseq string 0 split))))
          return t))

(defun %lint-a-duration-p (value)
  (and (stringp value) (plusp (length value))
       (or (%lint-a-clock-duration-p value)
           (%lint-a-duration-components-p value)
           (%lint-a-mixed-duration-p value))))

(defun %lint-invalid-effort-property (document context)
  (declare (ignore context))
  (loop for property in (%lint-nodes document :node-property)
        for key = (node-property property :key)
        for value = (node-property property :value)
        when (and (string= (or key "") "EFFORT")
                  (stringp value)
                  (plusp (length (string-trim '(#\Space #\Tab) value)))
                  (not (%lint-a-duration-p value))
                  (%lint-a-offset-p document (node-begin property)))
          collect (%lint-report document "invalid-effort-property"
                                (node-begin property)
                                (format nil "Invalid effort duration format: ~S" value))))

(defun %lint-invalid-id-property (document context)
  (declare (ignore context))
  (loop for property in (%lint-nodes document :node-property)
        for key = (node-property property :key)
        for value = (node-property property :value)
        when (and (string= (or key "") "ID")
                  (stringp value) (plusp (length value))
                  (search "::" value)
                  (%lint-a-offset-p document (node-begin property)))
          collect (%lint-report document "invalid-id-property"
                                (node-begin property)
                                (format nil "IDs should not include \"::\": ~S" value))))

(defun %lint-a-footnote-type-p (node type)
  (member (node-property node :type) (list type (string-downcase (symbol-name type)))
          :test #'equal))

(defun %lint-undefined-footnote-reference (document context)
  (declare (ignore context))
  (let* ((nodes (%lint-nodes document))
         (defined
           (loop for node in nodes
                 when (or (eq (node-type node) :footnote-definition)
                          (and (eq (node-type node) :footnote-reference)
                               (%lint-a-footnote-type-p node :inline)))
                   collect (node-property node :label))))
    (loop for reference in (%lint-nodes document :footnote-reference)
          for label = (node-property reference :label)
          when (and (%lint-a-offset-p document (node-begin reference))
                    (%lint-a-footnote-type-p reference :standard)
                    (not (member label defined :test #'equal)))
            collect (%lint-report document "undefined-footnote-reference"
                                  (node-begin reference)
                                  (format nil "Missing definition for footnote [~A]"
                                          label)))))

(defun %lint-unreferenced-footnote-definition (document context)
  (declare (ignore context))
  (let ((references
          (mapcar (lambda (node) (node-property node :label))
                  (%lint-nodes document :footnote-reference))))
    (loop for definition in (%lint-nodes document :footnote-definition)
          for label = (node-property definition :label)
          when (and label (not (member label references :test #'equal))
                    (%lint-a-offset-p document (node-post-affiliated definition)))
            collect (%lint-report document "unreferenced-footnote-definition"
                                  (node-post-affiliated definition)
                                  (format nil "No reference for footnote definition [~A]"
                                          label)))))

(defun %lint-extraneous-element-in-footnote-section (document context)
  (declare (ignore context))
  (let ((allowed (quote (:comment :comment-block :footnote-definition
                   :property-drawer :section))))
    (loop for heading in (%lint-nodes document :headline)
          for title = (node-property heading :raw-value)
          when (and (string= (or title "") "Footnotes")
                    (%lint-a-offset-p document (node-begin heading))
                    (some (lambda (node)
                            (and (not (eq node heading))
                                 (%lint-a-under-p node heading)
                                 (not (%lint-a-in-footnote-excluded-subtree-p
                                       node heading))
                                 (member (node-type node)
                                         (append (node-types :greater-element)
                                                 (node-types :lesser-element))
                                         :test (function eq))
                                 (not (member (node-type node) allowed
                                              :test (function eq)))
                                 (not (and (eq (node-type node) :headline)
                                           (node-property node :commentedp)))))
                          (%lint-nodes document)))
            collect (%lint-report
                     document "extraneous-element-in-footnote-section"
                     (node-begin heading)
                     "Extraneous elements in footnote section are not exported"))))
