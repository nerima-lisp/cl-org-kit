;;;; src/export-ascii.lisp
(in-package #:org-kit)

(defstruct (export-ascii-output
             (:constructor %make-export-ascii-output
                 (preparation limits output count base-count indent
                  downstream-indent line-start-p visible-controls-p display-width)))
  preparation
  limits
  output
  count
  base-count
  indent
  downstream-indent
  line-start-p
  visible-controls-p
  display-width)

(defun export-ascii-output-for-preparation
    (preparation base-count measure-only-p downstream-indent visible-controls-p)
  (%make-export-ascii-output
   preparation
   (document-limits (export-preparation-document preparation))
   (unless measure-only-p
     (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
   0 base-count 0 downstream-indent t visible-controls-p 0))

(defun export-ascii-visible-character-length (output character)
  (if (and (export-ascii-output-visible-controls-p output)
           (export-ascii-visible-control-character-p character))
      (if (<= #x80 (char-code character) #x9f) 6 4)
      1))

(defun export-ascii-output-reserve (output additional-length)
  (let* ((next-count (+ (export-ascii-output-count output) additional-length))
         (next-length (+ (export-ascii-output-base-count output) next-count))
         (limits (export-ascii-output-limits output)))
    (when (plusp additional-length)
      (check-limit limits :max-output-length next-length))
    (setf (export-ascii-output-count output) next-count)
    (let ((buffer (export-ascii-output-output output)))
      (when (and buffer (> next-count (array-total-size buffer)))
        (let* ((available (- (limit-value limits :max-output-length)
                             (export-ascii-output-base-count output)))
               (capacity (min available
                              (max next-count
                                   (if (zerop (array-total-size buffer))
                                       16
                                       (* 2 (array-total-size buffer)))))))
          (setf (export-ascii-output-output output)
                (adjust-array buffer capacity
                              :fill-pointer (fill-pointer buffer))))))
  output))

(defun export-ascii-output-push-char (output character)
  (let ((buffer (export-ascii-output-output output)))
    (when buffer
      (vector-push character buffer)))
  (incf (export-ascii-output-display-width output)
        (table-character-width character)))

(defun export-ascii-output-push-visible-char (output character)
  (let ((code (char-code character)))
    (if (and (export-ascii-output-visible-controls-p output)
             (export-ascii-visible-control-character-p character))
        (let ((digits (if (<= #x80 code #x9f) 4 2)))
          (export-ascii-output-push-char output #\\)
          (export-ascii-output-push-char output (if (= digits 4) #\u #\x))
          (loop for shift from (* 4 (1- digits)) downto 0 by 4
                for digit = (digit-char (ldb (byte 4 shift) code) 16)
                do (export-ascii-output-push-char output digit)))
        (export-ascii-output-push-char output character))))

(defun export-ascii-output-write-char (output character)
  (let* ((newline-p (char= character #\Newline))
         (indent-length (if (and (export-ascii-output-line-start-p output)
                                 (not newline-p))
                            (export-ascii-output-indent output)
                            0))
         (visible-length (export-ascii-visible-character-length output character)))
    (export-ascii-output-reserve output (+ indent-length visible-length))
    (loop repeat indent-length
          do (export-ascii-output-push-char output #\Space))
    (export-ascii-output-push-visible-char output character)
    (setf (export-ascii-output-line-start-p output) newline-p))
  output)

(defun export-ascii-output-range-length (output text start end
                                         &key uppercase-p substitute-tab-p)
  (loop with line-start-p = (export-ascii-output-line-start-p output)
        with length = 0
        with display-width = 0
        for index from start below end
        for source-character = (char text index)
        for character = (cond (substitute-tab-p
                               (if (char= source-character #\Tab)
                                   #\Space
                                   source-character))
                              (uppercase-p (char-upcase source-character))
                              (t source-character))
        for newline-p = (char= character #\Newline)
        do (when (and line-start-p (not newline-p))
             (incf length (export-ascii-output-indent output))
             (incf display-width (export-ascii-output-indent output)))
           (incf length (export-ascii-visible-character-length output character))
           (incf display-width
                 (if (and (export-ascii-output-visible-controls-p output)
                          (export-ascii-visible-control-character-p character))
                     (export-ascii-visible-character-length output character)
                     (table-character-width character)))
           (setf line-start-p newline-p)
        finally (return (values length line-start-p display-width))))

(defun export-ascii-output-write-range (output text start end
                                        &key uppercase-p substitute-tab-p)
  (multiple-value-bind (additional-length final-line-start-p ignored-width)
      (export-ascii-output-range-length
       output text start end :uppercase-p uppercase-p
       :substitute-tab-p substitute-tab-p)
    (declare (ignore ignored-width))
    (export-ascii-output-reserve output additional-length)
    (loop for index from start below end
          for source-character = (char text index)
          for character = (cond (substitute-tab-p
                                 (if (char= source-character #\Tab)
                                     #\Space
                                     source-character))
                                (uppercase-p (char-upcase source-character))
                                (t source-character))
          for newline-p = (char= character #\Newline)
          do (when (and (export-ascii-output-line-start-p output)
                        (not newline-p))
               (loop repeat (export-ascii-output-indent output)
                     do (export-ascii-output-push-char output #\Space)))
             (export-ascii-output-push-visible-char output character)
             (setf (export-ascii-output-line-start-p output) newline-p))
    (setf (export-ascii-output-line-start-p output) final-line-start-p))
  output)

(defun export-ascii-output-write-string (output text &key uppercase-p
                                                       substitute-tab-p)
  (export-ascii-output-write-range output text 0 (length text)
                                   :uppercase-p uppercase-p
                                   :substitute-tab-p substitute-tab-p))

(defun export-ascii-output-write-node-text (output node &key substitute-tab-p)
  (let ((document (node-document node))
        (start (node-begin node))
        (end (node-end node)))
    (if (and document start end)
        (export-ascii-output-write-range
         output (document-text document) start end
         :substitute-tab-p substitute-tab-p)
        (export-ascii-output-write-string
         output (or (node-text node) "")
         :substitute-tab-p substitute-tab-p))))

(defun export-ascii-output-write-repeat (output character count)
  (when (plusp count)
    (let* ((newline-p (char= character #\Newline))
           (indent-length (if (and (export-ascii-output-line-start-p output)
                                   (not newline-p))
                              (export-ascii-output-indent output)
                              0))
           (visible-length (* count
                             (export-ascii-visible-character-length
                              output character))))
      (export-ascii-output-reserve output (+ indent-length visible-length))
      (loop repeat indent-length
            do (export-ascii-output-push-char output #\Space))
      (loop repeat count
            do (export-ascii-output-push-visible-char output character))
      (setf (export-ascii-output-line-start-p output) newline-p)))
  output)

(defun export-ascii-output-with-indent (output width function)
  (let ((previous-indent (export-ascii-output-indent output)))
    (unwind-protect
         (progn
           (incf (export-ascii-output-indent output) width)
           (funcall function))
      (setf (export-ascii-output-indent output) previous-indent))))

(defun export-ascii-output-write-buffer (output source &optional (start 0)
                                                              (end (fill-pointer
                                                                    (export-ascii-output-output source))))
  (export-ascii-output-write-range output
                                   (export-ascii-output-output source)
                                   start end))

(defun export-ascii-output-string (output)
  (coerce (export-ascii-output-output output) 'string))

(defun export-ascii-output-newlines (output count)
  (export-ascii-output-write-repeat output #\Newline count))

(defun export-ascii-output-range-width (text start end)
  (loop for index from start below end
        for character = (char text index)
        sum (if (export-ascii-visible-control-character-p character)
                (if (<= #x80 (char-code character) #x9f) 6 4)
                (table-character-width character))))

(defun export-ascii-output-text-width (text)
  (export-ascii-output-range-width text 0 (length text)))

(defun export-ascii-output-scratch
    (output additional-base-count &key measure-only-p
                 (visible-controls-p
                   (export-ascii-output-visible-controls-p output)))
  (export-ascii-output-for-preparation
   (export-ascii-output-preparation output)
   (+ (export-ascii-output-base-count output)
      (export-ascii-output-count output)
      additional-base-count)
   measure-only-p
   (+ (export-ascii-output-downstream-indent output)
      (export-ascii-output-indent output))
   visible-controls-p))

(defun export-ascii-output-trim-newlines (output)
  (let ((buffer (export-ascii-output-output output)))
    (when buffer
      (loop while (and (plusp (fill-pointer buffer))
                       (char= (char buffer (1- (fill-pointer buffer)))
                              #\Newline))
            do (vector-pop buffer))
      (setf (export-ascii-output-count output) (fill-pointer buffer)
            (export-ascii-output-line-start-p output)
            (or (zerop (fill-pointer buffer))
                (char= (char buffer (1- (fill-pointer buffer))) #\Newline)))))
  output)

(defun export-ascii-output-write-trimmed-newlines (output text)
  (let ((end (length text)))
    (loop while (and (plusp end) (char= (char text (1- end)) #\Newline))
          do (decf end))
    (export-ascii-output-write-range output text 0 end)))

(defun export-ascii-output-write-wrapped-line
    (output text start end width &key uppercase-p)
  (let ((position start)
        (line-width 0)
        (word-start nil))
    (labels ((emit-word (word-end)
               (let ((word-width
                       (export-ascii-output-range-width text word-start word-end)))
                 (when (plusp line-width)
                   (if (> (+ line-width 1 word-width) width)
                       (progn
                         (export-ascii-output-newlines output 1)
                         (setf line-width 0))
                       (progn
                         (export-ascii-output-write-char output #\Space)
                         (incf line-width))))
                 (export-ascii-output-write-range
                  output text word-start word-end :uppercase-p uppercase-p)
                 (incf line-width word-width))))
      (loop while (< position end)
            for whitespace-p = (export-whitespace-p (char text position))
            do (if whitespace-p
                   (progn
                     (when word-start
                       (emit-word position)
                       (setf word-start nil))
                     (incf position))
                   (progn
                     (unless word-start (setf word-start position))
                     (incf position))))
      (when word-start (emit-word end)))))

(defun export-ascii-output-write-wrapped-range
    (output text start end width &key preserve-breaks uppercase-p)
  (if preserve-breaks
      (loop with line-start = start
            for newline = (position #\Newline text :start line-start :end end)
            for line-end = (or newline end)
            do (export-ascii-output-write-wrapped-line
                output text line-start line-end width :uppercase-p uppercase-p)
               (when newline
                 (export-ascii-output-newlines output 1)
                 (setf line-start (1+ newline)))
            while newline)
      (export-ascii-output-write-wrapped-line
       output text start end width :uppercase-p uppercase-p))
  output)

(defun export-ascii-option (options key default)
  (let ((entry (assoc key options)))
    (if entry (cdr entry) default)))



















(defun export-ascii-visible-control-character-p (character)
  (and (export-control-character-p character)
       (not (member character '(#\Newline #\Tab)))))



(defun export-ascii-node-value (node key)
  (or (node-property node key)
      (node-text node)
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















(defun export-ascii-footnote-number (label preparation)
  (let ((definition
          (find label (export-preparation-footnotes preparation)
                :key (lambda (entry) (getf entry :label))
                :test #'string=)))
    (getf definition :number)))





(defun export-ascii-local-heading (link preparation)
  (let* ((type (node-property link :type))
         (path (node-property link :path))
         (target-start (if (and path
                                (plusp (length path))
                                (char= (char path 0) #\#))
                           1
                           0))
         (found nil))
    (when (and path
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
                                (and found-p
                                     (string= value path
                                              :start2 target-start))))
                         (and (not (string= type "custom-id"))
                              (string-equal
                               (or (node-property candidate :raw-value) "")
                               path :start2 target-start)))
                     (null found))
            (setf found view))))
    found)))

(defun export-ascii-destination-source-safe-p (text start end)
  (and (stringp text)
       (loop for index from start below end
             for character = (char text index)
             never (or (export-control-character-p character)
                       (char= character #\\)))))

(defun export-ascii-destination-part-length (part)
  (if (and (consp part) (eq (first part) :range))
      (- (fourth part) (third part))
      (length part)))

(defun export-ascii-destination-part-character (part index)
  (if (and (consp part) (eq (first part) :range))
      (elt (second part) (+ (third part) index))
      (elt part index)))

(defun export-ascii-destination-parts-length (parts)
  (loop for part in parts sum (export-ascii-destination-part-length part)))

(defun export-ascii-destination-parts-character (parts index)
  (loop with remaining = index
        for part in parts
        for part-length = (export-ascii-destination-part-length part)
        when (< remaining part-length)
          do (return (export-ascii-destination-part-character part remaining))
        do (decf remaining part-length)))

(defun export-ascii-destination-parts-position (parts predicate)
  (loop for index below (export-ascii-destination-parts-length parts)
        for character = (export-ascii-destination-parts-character parts index)
        when (funcall predicate character) return index))

(defun export-ascii-destination-parts-prefix-equal-p (parts prefix)
  (and (>= (export-ascii-destination-parts-length parts) (length prefix))
       (loop for index below (length prefix)
             always (char= (export-ascii-destination-parts-character parts index)
                           (char prefix index)))))

(defun export-ascii-destination-parts-explicit-scheme-end (parts)
  (loop with position = 0
        with valid-p = t
        for part in parts
        do (loop for index below (export-ascii-destination-part-length part)
                 for character = (export-ascii-destination-part-character
                                  part index)
                 do (cond
                      ((char= character #\:)
                       (return-from
                           export-ascii-destination-parts-explicit-scheme-end
                         (and (plusp position) valid-p position)))
                      ((zerop position)
                       (unless (export-ascii-alpha-p character)
                         (setf valid-p nil)))
                      ((not (export-ascii-scheme-character-p character))
                       (setf valid-p nil)))
                    (incf position))))

(defun export-ascii-destination-parts-allowed-scheme-p (parts end)
  (loop for scheme in '("http" "https" "mailto")
        thereis (and (= (length scheme) end)
                     (loop for index below end
                           always (char-equal
                                   (export-ascii-destination-parts-character
                                    parts index)
                                   (char scheme index))))))

(defun export-ascii-destination-parts-safe-p (parts)
  (and (plusp (export-ascii-destination-parts-length parts))
       (not (export-ascii-destination-parts-position
             parts #'export-control-character-p))
       (not (export-ascii-destination-parts-position
             parts (lambda (character) (char= character #\\))))
       (not (export-whitespace-character-p
             (export-ascii-destination-parts-character parts 0)))
       (not (export-ascii-destination-parts-prefix-equal-p parts "//"))
       (let ((scheme-end
               (export-ascii-destination-parts-explicit-scheme-end parts)))
         (or (null scheme-end)
             (export-ascii-destination-parts-allowed-scheme-p
              parts scheme-end)))))

(defun export-ascii-destination-range-safe-p (text start end)
  (export-ascii-destination-parts-safe-p
   (list (list :range text start end))))

(defun export-ascii-link-file-path-p (path)
  (and (plusp (length path))
       (or (char= (char path 0) #\/)
           (and (>= (length path) 2)
                (string= "./" path :end2 2))
           (and (>= (length path) 3)
                (string= "../" path :end2 3)))))

(defun export-ascii-destination-safe-p
    (raw expanded &optional (raw-start 0) raw-end)
  (let ((raw-end (or raw-end (and (stringp raw) (length raw)))))
    (and raw-end
         (export-ascii-destination-source-safe-p raw raw-start raw-end)
         (stringp expanded)
         (export-ascii-destination-parts-safe-p (list expanded)))))

(defun export-ascii-link-source-path-range (node)
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
           (when close (values text start close))))
        (:angle
         (let* ((start (1+ begin))
                (close (position #\> text :start start :end end)))
           (when close (values text start close))))
        (:plain (values text begin end))))))

(defun export-ascii-output-write-bracket-path-range
    (output text start end)
  (let ((position start)
        (pending-start nil))
    (labels ((write-pending (pending-end)
               (when pending-start
                 (export-ascii-output-write-range
                  output text pending-start pending-end)
                 (setf pending-start nil))))
      (loop while (< position end)
            for character = (char text position)
            do (cond
                 ((member character '(#\Space #\Tab) :test #'char=)
                  (unless pending-start (setf pending-start position))
                  (incf position))
                 ((char= character #\Newline)
                  (setf pending-start nil)
                  (export-ascii-output-write-char output #\Space)
                  (incf position)
                  (loop while (and (< position end)
                                   (member (char text position)
                                           '(#\Space #\Tab) :test #'char=))
                        do (incf position)))
                 (t
                  (write-pending position)
                  (if (and (char= character #\\)
                           (< (1+ position) end)
                           (find (char text (1+ position)) "[]\\"))
                      (progn
                        (export-ascii-output-write-char
                         output (char text (1+ position)))
                        (incf position 2))
                      (progn
                        (export-ascii-output-write-char output character)
                        (incf position))))))
      (write-pending end)))
  output)

(defun export-ascii-link-abbreviation-entry (raw abbreviations)
  (let* ((separator (position #\: raw))
         (key-end (or separator (length raw))))
    (find-if (lambda (entry)
               (let ((key (car entry)))
                 (and (stringp key)
                      (= key-end (length key))
                      (string= raw key :end1 key-end))))
             abbreviations)))

(defun export-ascii-output-link-source-unsafe-p (node)
  (let ((source-path (node-property node :raw-link)))
    (multiple-value-bind (text start end)
        (if (stringp source-path)
            (values source-path 0 (length source-path))
            (export-ascii-link-source-path-range node))
      (when text
        (or (not (export-ascii-destination-source-safe-p text start end))
            (let* ((type (node-property node :type))
                   (path (node-property node :path))
                   (parts
                     (and (stringp path)
                          (if (and (stringp type)
                                   (member type *org-link-object-types*
                                           :test #'string=)
                                   (not (and (string= type "file")
                                             (export-ascii-link-file-path-p
                                              path))))
                              (list type ":" path)
                              (list path)))))
              (if parts
                  (not (export-ascii-destination-parts-safe-p parts))
                  (not (export-ascii-destination-range-safe-p
                        text start end)))))))))

(defun export-ascii-output-expand-link-abbreviation
    (output raw-output document)
  (let* ((raw (export-ascii-output-output raw-output))
         (entry (export-ascii-link-abbreviation-entry
                 raw (getf (document-settings document) :link-abbrevs)))
         (replacement (and entry (cdr entry))))
    (if (not (stringp replacement))
        raw-output
        (let* ((separator (position #\: raw))
               (tail-start
                 (and separator
                      (+ separator
                         (if (and (< (1+ separator) (length raw))
                                  (char= (char raw (1+ separator)) #\:))
                             2
                             1))))
               (placeholder (search "%s" replacement))
               (expanded (export-ascii-output-scratch
                          output 0 :visible-controls-p nil)))
          (if placeholder
              (progn
                (export-ascii-output-write-range
                 expanded replacement 0 placeholder)
                (when tail-start
                  (export-ascii-output-write-range
                   expanded raw tail-start (length raw)))
                (export-ascii-output-write-range
                 expanded replacement (+ placeholder 2) (length replacement)))
              (progn
                (export-ascii-output-write-string expanded replacement)
                (when tail-start
                  (export-ascii-output-write-range
                   expanded raw tail-start (length raw)))))
          expanded))))

(defun export-ascii-output-link-path
    (output view preparation)
  (let* ((node (export-view-node view))
         (type (node-property node :type))
         (path (node-property node :path))
         (source-path (node-property node :raw-link))
         (format (node-property node :format))
         (raw-output (export-ascii-output-scratch
                      output 0 :visible-controls-p nil))
         (raw-text nil)
         (raw-start 0)
         (raw-end nil))
    (cond
      ((stringp source-path)
       (setf raw-text source-path raw-end (length source-path))
       (if (eq format :bracket)
           (export-ascii-output-write-bracket-path-range
            raw-output source-path 0 (length source-path))
           (export-ascii-output-write-string raw-output source-path)))
      (t
       (multiple-value-bind (text start end)
           (export-ascii-link-source-path-range node)
         (if text
             (progn
               (setf raw-text text raw-start start raw-end end)
               (if (eq format :bracket)
                   (export-ascii-output-write-bracket-path-range
                    raw-output text start end)
                   (export-ascii-output-write-range
                    raw-output text start end)))
             (if (and path (stringp type)
                      (member type *org-link-object-types* :test #'string=))
                 (progn
                   (export-ascii-output-write-string raw-output type)
                   (export-ascii-output-write-char raw-output #\:)
                   (export-ascii-output-write-string raw-output path))
                 (export-ascii-output-write-string raw-output (or path "")))))))
    (let* ((raw (or raw-text (export-ascii-output-output raw-output)))
           (raw-end (or raw-end (length raw)))
           (expanded-output
             (export-ascii-output-expand-link-abbreviation
              output raw-output
              (export-preparation-document preparation)))
           (expanded (export-ascii-output-output expanded-output)))
      (values expanded-output
              (export-ascii-destination-safe-p
               raw expanded raw-start raw-end)))))









(defun export-ascii-item-ordered-p (node)
  (let* ((parent (node-parent node))
         (list-type (and parent (node-property parent :type)))
         (bullet (node-property node :bullet)))
    (or (eq list-type :ordered)
        (and (stringp bullet)
             (plusp (length bullet))
             (digit-char-p (char bullet 0))))))





























(defun export-ascii-output-gap-length (previous current)
  (let ((previous-type (node-type (export-view-node previous)))
        (current-type (node-type (export-view-node current))))
    (cond
      ((member current-type '(:headline :inlinetask) :test #'eq) 3)
      ((eq previous-type :plain-list) 1)
      (t (max 2 (1+ (export-ascii-view-blank-count previous)))))))

(defun export-ascii-output-write-gap (output previous current)
  (export-ascii-output-newlines
   output (export-ascii-output-gap-length previous current)))

(defun export-ascii-output-write-inline-views (output views preparation)
  (dolist (view views)
    (export-ascii-output-render-view output view preparation)
    (export-ascii-output-write-repeat
     output #\Space (node-post-blank (export-view-node view))))
  output)

(defun export-ascii-output-write-trimmed-plain-text-view
    (output view trim-leading-p trim-trailing-p)
  (let* ((node (export-view-node view))
         (document (node-document node))
         (start (node-begin node))
         (end (node-end node)))
    (when (and start end)
      (let ((text (document-text document)))
        (when trim-leading-p
          (loop while (and (< start end)
                           (find (char text start)
                                 '(#\Space #\Tab #\Newline #\Return)))
                do (incf start)))
        (when trim-trailing-p
          (loop while (and (> end start)
                           (find (char text (1- end))
                                 '(#\Space #\Tab #\Newline #\Return)))
                do (decf end)))
        (export-ascii-output-write-range
         output text start end :substitute-tab-p t)))))

(defun export-ascii-output-write-trimmed-inline-views
    (output views preparation)
  (loop with last-index = (1- (length views))
        for view in views
        for index from 0
        for node = (export-view-node view)
        for boundary-p = (or (zerop index) (= index last-index))
        do (if (and boundary-p (eq (node-type node) :plain-text))
               (export-ascii-output-write-trimmed-plain-text-view
                output view (zerop index) (= index last-index))
               (export-ascii-output-render-view output view preparation))
           (unless (= index last-index)
             (export-ascii-output-write-repeat
              output #\Space (node-post-blank node))))
  output)

(defun export-ascii-output-write-views (output views preparation)
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
        (let ((rendered
                (export-ascii-output-scratch
                 output (if previous
                            (export-ascii-output-gap-length previous view)
                            0))))
          (if (and included-heading-level
                   (not heading-p)
                   (not (eq type :section)))
              (export-ascii-output-with-indent
               rendered 2
               (lambda ()
                 (export-ascii-output-render-view
                  rendered view preparation)))
              (export-ascii-output-render-view rendered view preparation))
          (when (plusp (export-ascii-output-count rendered))
            (when previous
              (export-ascii-output-write-gap output previous view))
            (export-ascii-output-write-buffer output rendered)
            (setf previous view)))))
  output))

(defun export-ascii-output-measure-text-range
    (output text start end &key substitute-tab-p)
  (loop with output-length = 0
        with display-width = 0
        for index from start below end
        for source-character = (char text index)
        for character = (if (and substitute-tab-p (char= source-character #\Tab))
                            #\Space
                            source-character)
        for visible-length = (export-ascii-visible-character-length
                              output character)
        do (incf output-length visible-length)
           (incf display-width
                 (if (and (export-ascii-output-visible-controls-p output)
                          (export-ascii-visible-control-character-p character))
                     visible-length
                     (table-character-width character)))
        finally (return (values display-width output-length))))

(defun export-ascii-output-measure-table-cell (output cell preparation)
  (let* ((children (export-view-children cell))
         (node (export-view-node cell)))
    (cond
      ((null children)
       (export-ascii-output-measure-text-range
        output (or (node-property node :value) "") 0
        (length (or (node-property node :value) ""))))
      ((and (= (length children) 1)
            (eq (node-type (export-view-node (first children))) :plain-text)
            (node-document (export-view-node (first children)))
            (node-begin (export-view-node (first children)))
            (node-end (export-view-node (first children))))
       (let* ((child (export-view-node (first children)))
              (text (document-text (node-document child))))
         (multiple-value-bind (display-width output-length)
             (export-ascii-output-measure-text-range
              output text (node-begin child) (node-end child)
              :substitute-tab-p t)
           (let ((post-blank (node-post-blank child)))
             (values (+ display-width post-blank)
                     (+ output-length post-blank))))))
      (t
       (let ((measurement
               (export-ascii-output-scratch output 0 :measure-only-p t)))
         (export-ascii-output-write-inline-views
          measurement children preparation)
         (values (export-ascii-output-display-width measurement)
                 (export-ascii-output-count measurement)))))))

(defun export-ascii-output-check-table-width-budget (output widths width-hints)
  (let ((minimum-line-length
          (+ (export-ascii-output-indent output)
             (export-ascii-output-downstream-indent output)
             1
             (loop for column below (length widths)
                   sum (+ 3 (max 1 (aref widths column)
                                 (aref width-hints column)))))))
    (check-limit (export-ascii-output-limits output) :max-output-length
                 (+ (export-ascii-output-base-count output)
                    (export-ascii-output-count output)
                    minimum-line-length))))

(defun export-ascii-output-write-table-cell
    (output cell width alignment preparation &optional cell-metric)
  (multiple-value-bind (cell-width ignored-length)
      (if cell-metric
          (values (aref cell-metric 0) (aref cell-metric 1))
          (export-ascii-output-measure-table-cell output cell preparation))
    (declare (ignore ignored-length))
    (let* ((extra (max 0 (- width cell-width)))
           (left-padding (case alignment
                           (:right extra)
                           (:center (floor extra 2))
                           (otherwise 0)))
           (right-padding (- extra left-padding)))
      (export-ascii-output-write-char output #\Space)
      (export-ascii-output-write-repeat output #\Space left-padding)
      (if (export-view-children cell)
          (export-ascii-output-write-inline-views
           output (export-view-children cell) preparation)
          (export-ascii-output-write-string
           output (or (node-property (export-view-node cell) :value) "")))
      (export-ascii-output-write-repeat output #\Space right-padding)
      (export-ascii-output-write-char output #\Space)))
  output)

(defun export-ascii-output-cell-cookie-value (cell)
  (let* ((node (export-view-node cell))
         (value (node-property node :value))
         (children (export-view-children cell)))
    (or value
        (and (= (length children) 1)
             (let* ((child (export-view-node (first children)))
                    (text (node-text child)))
               (and (eq (node-type child) :plain-text) text))))))

(defun export-ascii-output-table-layout (output rows column-count preparation)
  (let* ((widths (make-array column-count :initial-element 1))
         (width-hints (make-array column-count :initial-element 0))
         (alignments (make-array column-count :initial-element nil))
         (row-metrics nil)
         (maximum-width
           (max 1 (- (limit-value (export-ascii-output-limits output)
                                 :max-output-length)
                     (export-ascii-output-base-count output)
                     (export-ascii-output-count output)))))
    (dolist (row rows)
      (unless (eq (node-property (export-view-node row) :type) :rule)
        (let* ((cells (export-view-children row))
               (metrics (make-array (length cells))))
          (loop for cell in cells
                for column from 0 below column-count
                do (let* ((candidate
                            (export-ascii-output-cell-cookie-value cell))
                          (alignment (and candidate
                                          (table-cell-alignment-cookie candidate)))
                          (width-hint
                            (and candidate
                                 (table-cell-width-cookie candidate maximum-width))))
                     (when (and alignment (null (aref alignments column)))
                       (setf (aref alignments column) alignment))
                     (when width-hint
                       (setf (aref width-hints column)
                             (max (aref width-hints column) width-hint))
                       (export-ascii-output-check-table-width-budget
                        output widths width-hints))
                     (multiple-value-bind (cell-width cell-length)
                         (export-ascii-output-measure-table-cell
                          output cell preparation)
                       (setf (aref widths column)
                             (max (aref widths column) cell-width)
                             (aref metrics column)
                             (vector cell-width cell-length)))))
          (push metrics row-metrics))))
    (loop for column below column-count
          do (setf (aref widths column)
                   (max (aref widths column) (aref width-hints column))))
    (values widths
            (map 'vector (lambda (alignment) (or alignment :left)) alignments)
            (coerce (nreverse row-metrics) 'vector))))

(defun export-ascii-output-table-row-length (row widths metrics)
  (if (eq (node-property (export-view-node row) :type) :rule)
      (+ 1 (loop for width across widths
                 sum (1+ (max 3 (+ width 2)))))
      (+ 1 (loop for metric across metrics
                 for column from 0
                 sum (+ 3
                        (max 0 (- (aref widths column) (aref metric 0)))
                        (aref metric 1))))))

(defun export-ascii-output-check-table-output-budget
    (output rows widths row-metrics)
  (let ((row-length
          (* (+ (export-ascii-output-indent output)
                (export-ascii-output-downstream-indent output))
             (length rows)))
        (metric-index 0))
    (dolist (row rows)
      (let* ((rule-p (eq (node-property (export-view-node row) :type) :rule))
             (metrics (unless rule-p
                        (prog1 (aref row-metrics metric-index)
                          (incf metric-index)))))
        (incf row-length
              (export-ascii-output-table-row-length row widths metrics))))
    (check-limit (export-ascii-output-limits output) :max-output-length
                 (+ (export-ascii-output-base-count output)
                    (export-ascii-output-count output)
                    row-length
                    (max 0 (1- (length rows)))))))

(defun export-ascii-output-write-table-row
    (output row widths alignments preparation &optional cell-metrics)
  (if (eq (node-property (export-view-node row) :type) :rule)
      (progn
        (export-ascii-output-write-char output #\+)
        (loop for width across widths
              do (export-ascii-output-write-repeat
                  output #\- (max 3 (+ width 2)))
                 (export-ascii-output-write-char output #\+)))
      (progn
        (export-ascii-output-write-char output #\|)
        (loop for cell in (export-view-children row)
              for column from 0
              do (export-ascii-output-write-table-cell
                  output cell (aref widths column) (aref alignments column)
                  preparation (and cell-metrics (aref cell-metrics column)))
                 (export-ascii-output-write-char output #\|))))
  output)

(defun export-ascii-output-write-table (output view preparation)
  (let* ((node (export-view-node view))
         (rows (export-view-children view))
         (table-type (node-property node :type)))
    (if (eq table-type :table.el)
        (export-ascii-output-write-trimmed-newlines
         output (or (node-property node :value) ""))
        (let* ((row-views
                 (remove-if
                  (lambda (row)
                    (eq (node-property (export-view-node row) :type) :rule))
                  rows))
               (column-count
                 (loop for row in row-views
                       maximize (length (export-view-children row)) into maximum
                       finally (return (or maximum 0)))))
          (multiple-value-bind (widths alignments row-metrics)
              (export-ascii-output-table-layout
               output row-views column-count preparation)
            (export-ascii-output-check-table-output-budget
             output rows widths row-metrics)
            (loop with metric-index = 0
                  for row in rows
                  for first-p = t then nil
                  for rule-p = (eq (node-property (export-view-node row) :type)
                                   :rule)
                  for cell-metrics = (unless rule-p
                                       (prog1 (aref row-metrics metric-index)
                                         (incf metric-index)))
                  do (unless first-p
                       (export-ascii-output-newlines output 1))
                     (export-ascii-output-write-table-row
                      output row widths alignments preparation cell-metrics))))))
  output)

(defun export-ascii-output-write-code-box
    (output value &key fixed-width-p)
  (let ((end (length value)))
    (loop while (and (plusp end) (char= (char value (1- end)) #\Newline))
          do (decf end))
    (unless (zerop end)
      (export-ascii-output-write-string output ",----")
      (export-ascii-output-newlines output 1)
      (loop with line-start = 0
            for newline = (position #\Newline value :start line-start :end end)
            for line-end = (or newline end)
            do (export-ascii-output-write-string output "| ")
               (let ((start line-start))
                 (when (and fixed-width-p (< start line-end)
                            (char= (char value start) #\:))
                   (incf start)
                   (loop while (and (< start line-end)
                                    (find (char value start) '(#\Space #\Tab)))
                         do (incf start)))
                 (export-ascii-output-write-range output value start line-end))
               (when newline
                 (export-ascii-output-newlines output 1)
                 (setf line-start (1+ newline)))
            while newline)
      (export-ascii-output-newlines output 1)
      (export-ascii-output-write-string output "`----")))
  output)

(defun export-ascii-output-write-heading-title
    (output view preparation numbers text-width &key underline toc)
  (let* ((node (export-view-node view))
         (options (export-preparation-options preparation))
         (level (or (node-property node :level) 1))
         (numbering (export-ascii-numbering-string view numbers options toc))
         (todo (and (export-ascii-option options :todo t)
                    (node-property node :todo-keyword)))
         (priority (and (export-ascii-option options :priority nil)
                        (node-property node :priority)))
         (title (or (node-property node :raw-value) ""))
         (priority-prefix (and priority (format nil "(##~A) " priority)))
         (first-width (+ (if numbering (export-ascii-output-text-width numbering) 0)
                         (if todo (+ (export-ascii-output-text-width todo) 1) 0)
                         (if priority-prefix
                             (export-ascii-output-text-width priority-prefix) 0)
                         (export-ascii-output-text-width title)))
         (tags-enabled (export-ascii-option options :tags t))
         (tags (and (not (and toc (eq tags-enabled :not-in-toc)))
                    tags-enabled
                    (effective-heading-tags
                     (export-view-source-document view) node :inherit nil)))
         (tag-width (if tags
                        (+ 2 (loop for tag in tags
                                   sum (export-ascii-output-text-width tag))
                           (max 0 (1- (length tags))))
                        0)))
    (when numbering (export-ascii-output-write-string output numbering))
    (when todo
      (export-ascii-output-write-string output todo)
      (export-ascii-output-write-char output #\Space))
    (when priority-prefix
      (export-ascii-output-write-string output priority-prefix))
    (export-ascii-output-write-string output title)
    (when tags
      (let* ((field-width (max (- text-width (1+ first-width)) tag-width))
             (space-count (1+ (- field-width tag-width))))
        (export-ascii-output-write-repeat output #\Space space-count)
        (export-ascii-output-write-char output #\:)
        (loop for tag in tags
              for first-p = t then nil
              do (unless first-p
                   (export-ascii-output-write-char output #\:))
                 (export-ascii-output-write-string output tag))
        (export-ascii-output-write-char output #\:)))
    (when (and underline (not toc) (<= level 3))
      (export-ascii-output-newlines output 1)
      (export-ascii-output-write-repeat
       output (nth (1- level) '(#\= #\~ #\-)) first-width)))
  output)

(defun export-ascii-output-write-plain-text-paragraph
    (output node preparation width)
  (let* ((document (node-document node))
         (text (and document (document-text document)))
         (start (node-begin node))
         (end (node-end node)))
    (when (and text start end)
      (loop while (and (< start end)
                       (find (char text start)
                             '(#\Space #\Tab #\Newline #\Return)))
            do (incf start))
      (loop while (and (> end start)
                       (find (char text (1- end))
                             '(#\Space #\Tab #\Newline #\Return)))
            do (decf end))
      (export-ascii-output-write-wrapped-range
       output text start end width
       :preserve-breaks
       (cdr (assoc :preserve-breaks
                   (export-preparation-options preparation))))
      t)))

(defun export-ascii-output-render-paragraph (output view preparation)
  (let* ((children (export-view-children view))
         (child (first children))
         (width (export-ascii-current-width (export-view-node view))))
    (unless (and (= (length children) 1)
                 (eq (node-type (export-view-node child)) :plain-text)
                 (export-ascii-output-write-plain-text-paragraph
                  output (export-view-node child) preparation width))
      (let ((inline (export-ascii-output-scratch output 0)))
        (export-ascii-output-write-trimmed-inline-views
         inline children preparation)
        (let* ((buffer (export-ascii-output-output inline))
               (start 0)
               (end (fill-pointer buffer)))
          (loop while (and (< start end)
                           (find (char buffer start)
                                 '(#\Space #\Tab #\Newline #\Return)))
                do (incf start))
          (loop while (and (> end start)
                           (find (char buffer (1- end))
                                 '(#\Space #\Tab #\Newline #\Return)))
                do (decf end))
          (export-ascii-output-write-wrapped-range
           output buffer start end width
           :preserve-breaks
           (cdr (assoc :preserve-breaks
                       (export-preparation-options preparation)))))))
  output))

(defun export-ascii-output-render-list-item
    (output view preparation ordinal)
  (let* ((node (export-view-node view))
         (parent (node-parent node))
         (list-type (and parent (node-property parent :type)))
         (children (export-view-children view))
         (first-child (first children))
         (descriptive-p (eq list-type :descriptive))
         (checkbox (case (node-property node :checkbox)
                     (:on "[X] ") (:off "[ ] ") (:trans "[-] ")
                     (otherwise "")))
         (bullet (or (node-property node :bullet) "- "))
         (tag-range (node-property node :reader-tag-range))
         (ordered-p (export-ascii-item-ordered-p node))
         (prefix-output (export-ascii-output-scratch output 0)))
    (cond
      (ordered-p
       (let* ((counter (node-property node :counter))
              (number (or counter ordinal))
              (punctuation (or (find-if (lambda (character)
                                         (find character ".)"))
                                       bullet)
                              #\.)))
         (export-ascii-output-write-string prefix-output
                                           (format nil "~D~C " number punctuation))))
      (descriptive-p
       (if tag-range
           (progn
             (export-ascii-output-write-range
              prefix-output (document-text (node-document node))
              (car tag-range) (cdr tag-range))
             (export-ascii-output-write-string prefix-output " :: "))
           (export-ascii-output-write-string prefix-output ":: ")))
      (t (export-ascii-output-write-string prefix-output bullet)))
    (export-ascii-output-write-string prefix-output checkbox)
    (let ((content (export-ascii-output-scratch output 0)))
      (export-ascii-output-write-views content children preparation)
      (cond
        ((zerop (export-ascii-output-count content))
         (export-ascii-output-write-buffer output prefix-output))
        ((and (not descriptive-p) first-child
              (eq (node-type (export-view-node first-child)) :paragraph))
         (let* ((buffer (export-ascii-output-output content))
                (end (fill-pointer buffer))
                (prefix-width (export-ascii-output-display-width prefix-output)))
           (loop while (and (plusp end)
                            (char= (char buffer (1- end)) #\Newline))
                 do (decf end))
           (loop with line-start = 0
                 for newline = (position #\Newline buffer
                                         :start line-start :end end)
                 for line-end = (or newline end)
                 do (if (zerop line-start)
                        (export-ascii-output-write-buffer output prefix-output)
                        (export-ascii-output-write-repeat
                         output #\Space prefix-width))
                    (export-ascii-output-write-range
                     output buffer line-start line-end)
                    (when newline
                      (export-ascii-output-newlines output 1)
                      (setf line-start (1+ newline)))
                 while newline)))
        (t
         (export-ascii-output-write-buffer output prefix-output)
         (export-ascii-output-newlines output 1)
         (export-ascii-output-with-indent
          output (if descriptive-p 6
                     (export-ascii-output-display-width prefix-output))
          (lambda ()
            (export-ascii-output-write-buffer output content)))))))
  output)

(defun export-ascii-output-render-list (output view preparation)
  (export-ascii-output-with-indent
   output 2
   (lambda ()
     (let ((ordinal 0))
       (loop for item in (export-view-children view)
             for first-p = t then nil
             for node = (export-view-node item)
             for ordered-p = (export-ascii-item-ordered-p node)
             do (if ordered-p (incf ordinal) (setf ordinal 0))
                (unless first-p (export-ascii-output-newlines output 1))
                (export-ascii-output-render-list-item
                 output item preparation ordinal)))))
  output)

(defun export-ascii-output-render-headline
    (output view preparation numbers)
  (let* ((node (export-view-node view))
         (level (or (node-property node :level) 1))
         (prefix (if (> level 3)
                     (concatenate
                      'string
                      (string (nth (mod (- level 4) 3) '(#\* #\+ #\-)))
                      " ")
                     ""))
         (body (export-ascii-output-scratch output 0)))
    (export-ascii-output-write-views
     body (export-view-children view) preparation)
    (export-ascii-output-write-string output prefix)
    (export-ascii-output-write-heading-title
     output view preparation numbers
     (max 1 (- 72 (if (> level 3) (* 2 (- level 3)) 0)))
     :underline t)
    (when (plusp (export-ascii-output-count body))
      (export-ascii-output-newlines output 2)
      (if (plusp (length prefix))
          (export-ascii-output-with-indent
           output (length prefix)
           (lambda () (export-ascii-output-write-buffer output body)))
          (export-ascii-output-write-buffer output body))))
  output)

(defun export-ascii-output-link-note (output view preparation)
  (let* ((node (export-view-node view))
         (type (node-property node :type))
         (children (export-view-children view))
         (target-view
           (and (member type '("custom-id" "fuzzy") :test #'string=)
                (export-ascii-local-heading node preparation))))
    (when (and children
               (not target-view)
               (not (member type '("radio" "custom-id") :test #'string=))
               (export-ascii-output-link-source-unsafe-p node))
      (return-from export-ascii-output-link-note nil))
    (when children
      (let ((description (export-ascii-output-scratch output 0)))
        (export-ascii-output-write-inline-views
         description children preparation)
        (when (plusp (export-ascii-output-count description))
          (cond
            ((string= (or type "") "radio") nil)
            (target-view
             (let* ((target (export-view-node target-view))
                    (numbers (export-ascii-numbering preparation))
                    (numbering (gethash target-view numbers))
                    (label-output (export-ascii-output-scratch output 0))
                    (note (export-ascii-output-scratch output 0)))
               (if (and numbering
                        (export-ascii-numbering-enabled-p
                         (export-preparation-options preparation)
                         (or (node-property target :level) 1)))
                   (export-ascii-output-write-string
                    label-output (format nil "~{~D~^.~}" numbering))
                   (export-ascii-output-write-string
                    label-output (or (node-property target :raw-value) "")))
               (export-ascii-output-write-char note #\[)
               (export-ascii-output-write-buffer note description)
               (export-ascii-output-write-string note "] See section ")
               (export-ascii-output-write-buffer note label-output)
               (values note nil)))
            ((string= (or type "") "custom-id") nil)
            (t
             (multiple-value-bind (expanded safe-p)
                 (export-ascii-output-link-path output view preparation)
               (when safe-p
                 (let ((note (export-ascii-output-scratch output 0)))
                   (export-ascii-output-write-char note #\[)
                   (export-ascii-output-write-buffer note description)
                   (export-ascii-output-write-string note "] <")
                   (export-ascii-output-write-buffer note expanded)
                   (export-ascii-output-write-char note #\>)
                   (values note t)))))))))))

(defun export-ascii-output-write-link-notes (output view preparation)
  (let ((seen (make-hash-table :test #'equal))
        (first-p t))
    (dolist (candidate
             (labels ((collect (current)
                        (if (member (node-type (export-view-node current))
                                    '(:headline :inlinetask) :test #'eq)
                            nil
                            (cons current
                                  (mapcan #'collect
                                          (export-view-children current))))))
               (collect view)))
      (when (eq (node-type (export-view-node candidate)) :link)
        (multiple-value-bind (note wrapped-p)
            (export-ascii-output-link-note output candidate preparation)
          (when note
            (let ((buffer (export-ascii-output-output note)))
              (unless (gethash buffer seen)
                (setf (gethash buffer seen) t)
                (unless first-p
                  (export-ascii-output-newlines output 2))
                (when first-p
                  (export-ascii-output-newlines output 3)
                  (setf first-p nil))
                (if wrapped-p
                    (export-ascii-output-write-wrapped-range
                     output buffer 0 (fill-pointer buffer) 72)
                    (export-ascii-output-write-buffer output note))))))))
  output))

(defun export-ascii-output-render-section (output view preparation)
  (export-ascii-output-with-indent
   output (export-ascii-section-margin (export-view-node view))
   (lambda ()
     (export-ascii-output-write-views
      output (export-view-children view) preparation)))
  (export-ascii-output-write-link-notes output view preparation)
  output)

(defun export-ascii-output-render-source-block
    (output view preparation)
  (let* ((node (export-view-node view))
         (mode (getf (export-view-payload view) :exports))
         (source (or (node-property node :value) ""))
         (source-p (not (member mode '(:none :results) :test #'eq)))
         (results-p (not (member mode '(:none :code) :test #'eq)))
         (results (and results-p (export-ascii-output-scratch output 0))))
    (when results-p
      (export-ascii-output-write-views
       results (export-view-children view) preparation))
    (let ((source-end (length source)))
      (loop while (and (plusp source-end)
                       (char= (char source (1- source-end)) #\Newline))
            do (decf source-end))
      (when (and source-p (plusp source-end))
        (export-ascii-output-write-code-box output source)
        (when (and results (plusp (export-ascii-output-count results)))
          (export-ascii-output-newlines output 2)))
      (when (and results (plusp (export-ascii-output-count results)))
        (export-ascii-output-write-buffer output results)))
    output))

(defun export-ascii-output-render-link (output view preparation)
  (let* ((node (export-view-node view))
         (children (export-view-children view))
         (type (node-property node :type))
         (target-view
           (and (member type '("custom-id" "fuzzy") :test #'string=)
                (export-ascii-local-heading node preparation))))
    (cond
      ((string= (or type "") "radio")
       (export-ascii-output-write-inline-views output children preparation))
      (target-view
       (if children
           (progn
             (export-ascii-output-write-char output #\[)
             (export-ascii-output-write-inline-views output children preparation)
             (export-ascii-output-write-char output #\]))
           (export-ascii-output-write-string
            output (or (node-property (export-view-node target-view) :raw-value)
                       ""))))
      ((string= (or type "") "custom-id")
       (if children
           (progn
             (export-ascii-output-write-char output #\[)
             (export-ascii-output-write-inline-views output children preparation)
             (export-ascii-output-write-char output #\]))
           (export-ascii-output-write-string output
                                             (or (node-property node :path) ""))))
      (children
       (export-ascii-output-write-char output #\[)
       (export-ascii-output-write-inline-views output children preparation)
       (export-ascii-output-write-char output #\]))
      (t
       (multiple-value-bind (expanded safe-p)
           (export-ascii-output-link-path output view preparation)
         (when safe-p (export-ascii-output-write-char output #\<))
         (export-ascii-output-write-buffer output expanded)
         (when safe-p (export-ascii-output-write-char output #\>))))))
  output)

(defun export-ascii-output-render-view (output view preparation)
  (let* ((node (export-view-node view))
         (type (node-type node))
         (children (export-view-children view))
         (numbers (and (member type '(:headline :inlinetask) :test #'eq)
                       (export-ascii-numbering preparation))))
    (case type
      ((:org-data :special-block :drawer :center-block :macro)
       (export-ascii-output-write-views output children preparation))
      ((:headline :inlinetask)
       (export-ascii-output-render-headline output view preparation numbers))
      (:section (export-ascii-output-render-section output view preparation))
      (:paragraph
       (export-ascii-output-render-paragraph output view preparation))
      (:plain-list (export-ascii-output-render-list output view preparation))
      (:table (export-ascii-output-write-table output view preparation))
      ((:babel-call :inline-babel-call :citation :citation-reference)
       (export-ascii-output-write-node-text output node))
      ((:example-block :src-block)
       (if (eq type :src-block)
           (export-ascii-output-render-source-block output view preparation)
           (export-ascii-output-write-code-box
            output (export-ascii-node-value node :value))))
      (:fixed-width
       (export-ascii-output-write-code-box
        output (export-ascii-node-value node :value) :fixed-width-p t))
      (:dynamic-block
       (export-ascii-output-write-string output
                                         (export-ascii-node-value node :value)))
      (:export-block
       (when (string-equal (or (node-property node :type) "") "ASCII")
         (export-ascii-output-write-string
          output (export-ascii-node-value node :value))))
      (:export-snippet
       (when (string-equal (or (node-property node :back-end) "") "ascii")
         (export-ascii-output-write-string
          output (or (node-property node :value) ""))))
      (:horizontal-rule
       (export-ascii-output-write-repeat output #\- 72))
      (:footnote-definition nil)
      ((:bold :italic)
       (let ((delimiter (if (eq type :bold) #\* #\/)))
         (export-ascii-output-write-char output delimiter)
         (export-ascii-output-write-inline-views output children preparation)
         (export-ascii-output-write-char output delimiter)))
      ((:strike-through :underline :subscript :superscript)
       (export-ascii-output-write-inline-views output children preparation))
      ((:code :verbatim)
       (export-ascii-output-write-char output #\`)
       (export-ascii-output-write-string
        output (or (node-property node :value) ""))
       (export-ascii-output-write-char output #\'))
      (:entity
       (let ((value (or (node-property node :utf-8)
                        (node-property node :utf8)
                        (node-property node :ascii))))
         (if value
             (export-ascii-output-write-string output value)
             (export-ascii-output-write-node-text output node))))
      (:footnote-reference
       (let ((label (node-property node :label)))
         (if label
             (progn
               (export-ascii-output-write-char output #\[)
               (export-ascii-output-write-string
                output (princ-to-string
                        (export-ascii-footnote-number label preparation)))
               (export-ascii-output-write-char output #\]))
             (export-ascii-output-write-inline-views
              output children preparation))))
      (:inline-src-block
       (export-ascii-output-write-string output (or (node-property node :value) "")))
      (:line-break (export-ascii-output-newlines output 1))
      (:link (export-ascii-output-render-link output view preparation))
      (:plain-text
       (export-ascii-output-write-node-text
        output node :substitute-tab-p t))
      ((:radio-target :target) nil)
      ((:statistics-cookie :timestamp)
       (export-ascii-output-write-node-text output node))
      ((:quote-block :verse-block)
       (export-ascii-output-with-indent
        output 6
        (lambda ()
          (export-ascii-output-write-views output children preparation))))
      ((:latex-environment :latex-fragment)
       (export-ascii-output-write-string
        output (export-ascii-node-value node :value)))
      (otherwise
       (export-ascii-output-write-views output children preparation))))
  output)

(defun export-ascii-output-write-centered-buffer (output buffer width)
  (loop with start = 0
        while (< start (length buffer))
        for newline = (position #\Newline buffer :start start)
        for end = (or newline (length buffer))
        for line-width = (export-ascii-output-range-width buffer start end)
        do (when (and (plusp line-width) (< line-width width))
             (export-ascii-output-write-repeat
              output #\Space (floor (- width line-width) 2)))
           (export-ascii-output-write-range output buffer start end)
           (when newline
             (export-ascii-output-newlines output 1)
             (setf start (1+ newline)))
        while newline)
  output)

(defun export-ascii-output-write-document-title (output preparation)
  (let* ((metadata (export-preparation-metadata preparation))
         (options (export-preparation-options preparation))
         (title (or (and (export-ascii-option options :title t)
                         (getf metadata :title)) ""))
         (author (or (and (export-ascii-option options :author t)
                          (getf metadata :author)) ""))
         (date (or (and (export-ascii-option options :date t)
                        (getf metadata :date)) "")))
    (cond
      ((plusp (length title))
       (let* ((title-width (min (export-ascii-output-text-width title) 48))
              (line-width
                (min 72 (+ 2 (max title-width
                                  (export-ascii-output-text-width author)))))
              (block (export-ascii-output-scratch output 0)))
         (export-ascii-output-write-repeat block #\_ line-width)
         (export-ascii-output-newlines block 2)
         (export-ascii-output-write-wrapped-range
          block title 0 (length title) title-width :uppercase-p t)
         (when (plusp (length author))
           (export-ascii-output-newlines block 2)
           (export-ascii-output-write-string block author))
         (export-ascii-output-newlines block 1)
         (export-ascii-output-write-repeat block #\_ line-width)
         (when (plusp (length date))
           (export-ascii-output-newlines block 3)
           (export-ascii-output-write-string block date))
         (export-ascii-output-newlines block 3)
         (export-ascii-output-write-centered-buffer
          output (export-ascii-output-output block) 72)))
      ((and (plusp (length author)) (plusp (length date)))
       (export-ascii-output-write-string output author)
       (export-ascii-output-write-repeat
        output #\Space
        (max 1 (- 72 (export-ascii-output-text-width author)
                  (export-ascii-output-text-width date))))
       (export-ascii-output-write-string output date)
       (export-ascii-output-newlines output 3))
      ((plusp (length date))
       (export-ascii-output-write-repeat
        output #\Space
        (max 0 (- 72 (export-ascii-output-text-width date))))
       (export-ascii-output-write-string output date)
       (export-ascii-output-newlines output 3))
      ((plusp (length author))
       (export-ascii-output-write-string output author)
       (export-ascii-output-newlines output 3))))
  output)

(defun export-ascii-output-write-toc (output preparation numbers)
  (let ((entries (export-preparation-toc preparation)))
    (when entries
      (export-ascii-output-write-string
       output "Table of Contents")
      (export-ascii-output-newlines output 1)
      (export-ascii-output-write-string output "_________________")
      (export-ascii-output-newlines output 2)
      (loop for entry in entries
            for first-p = t then nil
            for view = (getf entry :view)
            for level = (getf entry :level)
            for indent = (* (1- level) 3)
            do (unless first-p (export-ascii-output-newlines output 1))
               (when (plusp indent)
                 (export-ascii-output-write-repeat output #\. (1- indent))
                 (export-ascii-output-write-char output #\Space))
               (export-ascii-output-write-heading-title
                output view preparation numbers (- 72 indent) :toc t))
      (export-ascii-output-newlines output 3)))
  output)

(defun export-ascii-output-write-footnotes (output preparation)
  (let ((footnotes (export-preparation-footnotes preparation)))
    (when footnotes
      (export-ascii-output-newlines output 4)
      (export-ascii-output-write-string output "Footnotes")
      (export-ascii-output-newlines output 1)
      (export-ascii-output-write-string output "_________")
      (export-ascii-output-newlines output 2)
      (loop for definition in footnotes
            for first-p = t then nil
            for number = (getf definition :number)
            for view = (getf definition :definition)
            do (unless first-p (export-ascii-output-newlines output 2))
               (let ((content (export-ascii-output-scratch output 0))
                     (line (export-ascii-output-scratch output 0)))
                 (export-ascii-output-write-views
                  content (export-view-children view) preparation)
                 (let* ((buffer (export-ascii-output-output content))
                        (start 0)
                        (end (fill-pointer buffer)))
                   (loop while (and (< start end)
                                    (find (char buffer start)
                                          '(#\Space #\Tab #\Newline #\Return #\Page)))
                         do (incf start))
                   (loop while (and (> end start)
                                    (find (char buffer (1- end))
                                          '(#\Space #\Tab #\Newline #\Return #\Page)))
                         do (decf end))
                   (export-ascii-output-write-char line #\[)
                   (export-ascii-output-write-string line (princ-to-string number))
                   (export-ascii-output-write-string line "] ")
                   (export-ascii-output-write-range line buffer start end))
                 (export-ascii-output-write-wrapped-range
                  output (export-ascii-output-output line) 0
                  (fill-pointer (export-ascii-output-output line)) 72)))))
  output)

(defun export-ascii-render-preparation (preparation)
  ;; Reserve the final newline while nested views preflight their output.
  (let* ((output (export-ascii-output-for-preparation
                  preparation 1 nil 0 t))
         (numbers (export-ascii-numbering preparation)))
    (export-ascii-output-write-document-title output preparation)
    (export-ascii-output-write-toc output preparation numbers)
    (export-ascii-output-write-views
     output (export-preparation-root-views preparation) preparation)
    (export-ascii-output-write-footnotes output preparation)
    (export-ascii-output-trim-newlines output)
    ;; The reserved character becomes the newline written below.
    (decf (export-ascii-output-base-count output))
    (export-ascii-output-newlines output 1)
    (export-ascii-output-string output)))

(defmethod export-render-preparation
    ((preparation export-preparation) (backend (eql :ascii)))
  (declare (ignore backend))
  (export-ascii-render-preparation preparation))
