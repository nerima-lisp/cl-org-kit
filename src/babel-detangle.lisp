;;;; src/babel-detangle.lisp
(in-package #:org-kit)

(defun babel-detangle-line-bounds (text start)
  "Return the content end and next line start for TEXT at START."
  (let ((newline (position #\Newline text :start start)))
    (if newline
        (values (if (and (> newline start)
                         (char= (char text (1- newline)) #\Return))
                    (1- newline)
                    newline)
                (1+ newline))
        (values (length text) (length text)))))

(defun babel-detangle-trailing-wrapper-p (text)
  (member (string-trim (list #\Space #\Tab #\Return) text)
          (list "" "*/" "-->" "*)" "-}" "|#")
          :test (function string=)))

(defun babel-detangle-link-marker (line)
  "Return the source name and target from a tangle link comment line."
  (let* ((opening (search "[[" line))
         (separator (and opening
                         (search "][" line :start2 (+ opening 2))))
         (closing (and separator
                       (search "]]" line :start2 (+ separator 2)))))
    (when (and opening separator closing
               (babel-detangle-trailing-wrapper-p
                (subseq line (+ closing 2))))
      (let ((target (subseq line (+ opening 2) separator))
            (source-name (subseq line (+ separator 2) closing)))
        (when (and (plusp (length target)) (plusp (length source-name)))
          (values source-name target t))))))

(defun babel-detangle-line-marker-p (line marker consume-work)
  (loop with start = 0
        for position = (search marker line :start2 start)
        do (funcall consume-work (+ (length line) (length marker)))
           (unless position
             (return nil))
           (let ((end (+ position (length marker)))
                 (previous (and (plusp position)
                                (char line (1- position)))))
             (when (and (or (null previous)
                            (char= previous #\Space)
                            (char= previous #\Tab)
                            (char= previous #\Return)
                            (not (alphanumericp previous)))
                        (babel-detangle-trailing-wrapper-p
                         (subseq line end)))
               (return t)))
           (setf start (1+ position))))

(defun babel-detangle-end-marker-name (line source-names consume-work)
  (let ((match nil))
    (dolist (source-name source-names match)
      (let ((marker (concatenate 'string source-name " ends here")))
        (when (and (or (null match)
                       (> (length source-name) (length match)))
                   (babel-detangle-line-marker-p line marker consume-work))
          (setf match source-name))))))

(defun babel-detangle-index-source-blocks (document consume-work)
  (let ((named (make-hash-table :test #'equal))
        (generated (make-hash-table :test #'equal))
        (heading-counts (make-hash-table :test #'eq)))
    (map-nodes
     (lambda (node)
       (funcall consume-work 1)
       (when (eq (node-type node) :src-block)
         (let* ((name (node-property node :name))
                (heading (babel-containing-heading node))
                (bucket (or heading :no-heading))
                (heading-name (and heading
                                   (node-property heading :raw-value)))
                (source-heading (or heading-name "No heading"))
                (number (1+ (gethash bucket heading-counts 0))))
           (setf (gethash bucket heading-counts) number)
           (funcall consume-work (+ (if (stringp name) (length name) 0) (length source-heading)))
           (if (and (stringp name) (plusp (length name)))
               (push node (gethash name named))
               (push node (gethash (format nil "~A:~D" source-heading number)
                                   generated))))))
     (document-root document))
    (maphash (lambda (name blocks)
               (setf (gethash name named) (nreverse blocks)))
             named)
    (maphash (lambda (name blocks)
               (setf (gethash name generated) (nreverse blocks)))
             generated)
    (values named generated)))

(defun babel-detangle-source-names (named generated)
  (let ((names nil))
    (dolist (index (list named generated))
      (maphash (lambda (name blocks)
                 (declare (ignore blocks))
                 (pushnew name names :test #'string=))
               index))
    names))

(defun babel-detangle-resolve-block (source-name named generated)
  (let ((candidates (append (gethash source-name named)
                            (gethash source-name generated))))
    (cond
      ((null candidates) nil)
      ((cdr candidates)
       (babel-tangle-signal-error
        :ambiguous-detangle-source-name
        "A detangle link resolves to more than one source block"
        source-name))
      (t (car candidates)))))

(defun babel-detangle-sections (document tangled-content consume-work)
  (multiple-value-bind (named generated)
      (babel-detangle-index-source-blocks document consume-work)
    (let ((source-names (babel-detangle-source-names named generated))
          (sections nil)
          (active nil)
          (position 0))
      (loop while (< position (length tangled-content))
            for line-start = position
            do (multiple-value-bind (line-end next)
                   (babel-detangle-line-bounds tangled-content position)
                 (let ((line (subseq tangled-content line-start line-end)))
                   (funcall consume-work (- next line-start))
                   (multiple-value-bind (source-name target link-p)
                       (babel-detangle-link-marker line)
                     (declare (ignore target))
                     (cond
                       (active
                        (when link-p
                          (babel-tangle-signal-error
                           :malformed-detangle-section
                           "A linked section begins before the previous section ends"
                           (getf active :source-name)))
                        (when (search " ends here" line)
                          (let ((end-name
                                  (babel-detangle-end-marker-name
                                   line source-names consume-work)))
                            (when end-name
                              (unless (string= end-name
                                               (getf active :source-name))
                                (babel-tangle-signal-error
                                 :malformed-detangle-section
                                 "A detangle section ends with a different source name"
                                 end-name))
                              (push (list :source-name end-name
                                          :block (getf active :block)
                                          :body-start (getf active :body-start)
                                          :body-end line-start)
                                    sections)
                              (setf active nil)))))
                       (link-p
                        (let ((block (babel-detangle-resolve-block
                                      source-name named generated)))
                          (when block
                            (setf active
                                  (list :source-name source-name
                                        :block block
                                        :body-start next)))))))
                   (setf position next))))
      (when active
        (babel-tangle-signal-error
         :malformed-detangle-section
         "A detangle link has no matching end marker"
         (getf active :source-name)))
      (nreverse sections))))

(defun babel-detangle-patches (document tangled-content sections)
  (let ((replacements (make-hash-table :test #'eq))
        (patches nil))
    (dolist (section sections)
      (let* ((block (getf section :block))
             (replacement
               (subseq tangled-content
                       (getf section :body-start)
                       (getf section :body-end))))
        (multiple-value-bind (previous found-p) (gethash block replacements)
          (when (and found-p (not (string= previous replacement)))
            (babel-tangle-signal-error
             :ambiguous-detangle-source-name
             "Multiple linked sections provide different bodies for one source block"
             (getf section :source-name)))
          (unless found-p
            (setf (gethash block replacements) replacement)
            (let ((old-body (node-property block :value)))
              (unless (string= old-body replacement)
                (multiple-value-bind (start end)
                    (edit-element-block-body-span
                     document block old-body (getf section :source-name))
                  (push (edit-make-patch start end replacement block) patches))))))))
    (values (nreverse patches) replacements)))

(defun babel-detangle (document tangled-content)
  "Return an edit result that copies linked tangling output into DOCUMENT.

TANGLED-CONTENT is scanned for Org link comments and their matching
source-name end markers. Only the matched source-block bodies are replaced;
no file is read or written and no process is started. Unmatched links are left
alone. Ambiguous source names and malformed linked sections signal
ORG-BABEL-ERROR."
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (document-root document)
    (babel-tangle-signal-error :unparsed-document
                               "Detangling requires a parsed document" nil))
  (unless (stringp tangled-content)
    (error 'type-error :datum tangled-content :expected-type 'string))
  (let* ((limits (document-limits document))
         (source (document-text document))
         (search-work 0))
    (check-limit limits :max-input-length (length source))
    (check-limit limits :max-input-length (length tangled-content))
    (labels ((consume-work (amount)
               (incf search-work amount)
               (check-limit limits :max-search-work search-work)))
      (consume-work (length source))
      (let ((sections (babel-detangle-sections
                       document tangled-content #'consume-work)))
        (multiple-value-bind (patches replacements)
            (babel-detangle-patches document tangled-content sections)
          (let ((expected
                  (edit-canonical-copy (edit-canonical-document document))))
            (maphash
             (lambda (block replacement)
               (edit-canonical-set-property
                expected (edit-node-path document block "Babel detangle")
                :value replacement))
             replacements)
            (edit-apply-patches-result document patches
                                       :target "Babel detangle"
                                       :expected expected))))))
)
