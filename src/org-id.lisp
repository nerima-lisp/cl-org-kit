;;;; UUID-based Org IDs and document-local ID indexing.
(in-package #:org-kit)

(define-condition org-id-error (org-kit-error)
  ((reason :initarg :reason :initform nil))
  (:report (lambda (condition stream)
             (format stream "~A (~(~A~))"
                     (org-kit-error-message condition)
                     (org-id-error-reason condition))))
  (:documentation "Signalled when an Org ID input or operation is invalid.
REASON classifies the failure."))

(defun org-id-error-reason (condition)
  "Return the keyword classifying why the Org ID operation was refused."
  (slot-value condition 'reason))

(defun org-id-signal-error (reason message)
  (error 'org-id-error :reason reason :message message))

(defun org-id-valid-value-p (value)
  (and (stringp value)
       (plusp (length value))
       (not (every (lambda (character)
                     (find character '(#\Space #\Tab)))
                   value))
       (not (find #\Newline value))
       (not (find #\Return value))))

(defun org-id-check-value (value)
  (unless (org-id-valid-value-p value)
    (org-id-signal-error :invalid-id
                         "Org ID must be a non-empty single-line string"))
  value)

(defun org-id-normalize-documents (documents)
  (let ((normalized (if (document-p documents) (list documents) documents)))
    (unless (edit-proper-list-length normalized)
      (org-id-signal-error :invalid-documents
                           "Expected a document or a proper list of documents"))
    (unless (every #'document-p normalized)
      (org-id-signal-error :invalid-document
                           "Every Org ID index input must be a document"))
    normalized))

(defun org-id-target-id-properties (target)
  (effective-heading-property-entries target "ID"))

(defun org-id-document-id-targets (document)
  (let ((targets nil))
    (when (document-root document)
      (map-nodes (lambda (node)
                   (when (member (node-type node) '(:headline :inlinetask)
                                 :test #'eq)
                     (push node targets)))
                 (document-root document)))
    (nreverse targets)))

(defun org-id-document-identifiers (document &key reject-duplicates)
  (let ((identifiers nil)
        (seen (make-hash-table :test #'equal)))
    (dolist (target (org-id-document-id-targets document))
      (let ((properties (org-id-target-id-properties target)))
        (when (and reject-duplicates (> (length properties) 1))
          (org-id-signal-error :duplicate-id
                               "A target has more than one ID property"))
        (dolist (property properties)
          (let ((id (org-id-check-value (node-property property :value))))
            (when (and reject-duplicates (gethash id seen))
              (org-id-signal-error :duplicate-id
                                   "An ID occurs more than once in the document"))
            (setf (gethash id seen) t)
            (push (cons id target) identifiers)))))
    (nreverse identifiers)))

(defvar *org-id-random-state-factory*
  (lambda () (make-random-state t)))

(defun org-id-default-random-source ()
  (let ((random-state (funcall *org-id-random-state-factory*)))
    (lambda () (random 256 random-state))))

(defun generate-org-id (&key (random-source nil random-source-supplied-p))
  "Return a lowercase UUID version 4 string.
RANDOM-SOURCE, when supplied, must be a zero-argument function returning
successive integer octets from 0 through 255. Without it, a fresh random state
is created for each UUID. Signals ORG-ID-ERROR when the source is not a function
or returns an invalid octet; errors signalled by RANDOM-SOURCE propagate."
  (unless random-source-supplied-p
    (setf random-source (org-id-default-random-source)))
  (unless (functionp random-source)
    (org-id-signal-error :invalid-random-source
                         "Random source must be a zero-argument function"))
  (let ((octets (make-array 16 :element-type '(unsigned-byte 8)))
        (hex "0123456789abcdef"))
    (dotimes (index 16)
      (let ((octet (funcall random-source)))
        (unless (typep octet '(integer 0 255))
          (org-id-signal-error :invalid-random-octet
                               "Random source must return octets from 0 through 255"))
        (setf (aref octets index) octet)))
    (setf (aref octets 6) (logior #x40 (logand #x0f (aref octets 6)))
          (aref octets 8) (logior #x80 (logand #x3f (aref octets 8))))
    (with-output-to-string (stream)
      (loop for index from 0 below 16
            for octet = (aref octets index)
            do (when (member index '(4 6 8 10))
                 (write-char #\- stream))
               (write-char (char hex (ash octet -4)) stream)
               (write-char (char hex (logand octet #x0f)) stream)))))

(defun set-heading-id (document heading &key (random-source nil random-source-supplied-p))
  "Set a newly generated UUID on HEADING and return the updated document and ID.
RANDOM-SOURCE is passed to GENERATE-ORG-ID when supplied. Signals ORG-ID-ERROR
for invalid document, heading, or local ID data, and when the generated ID is
already present in DOCUMENT."
  (unless (document-p document)
    (org-id-signal-error :invalid-document "Expected an Org document"))
  (unless (node-p heading)
    (org-id-signal-error :invalid-heading "Expected a headline node"))
  (unless (eq (node-document heading) document)
    (org-id-signal-error :foreign-heading
                         "Headline does not belong to the input document"))
  (unless (eq (node-type heading) :headline)
    (org-id-signal-error :invalid-heading "Expected a headline node"))
  (let* ((identifiers (org-id-document-identifiers document :reject-duplicates t))
         (id (if random-source-supplied-p
                 (generate-org-id :random-source random-source)
                 (generate-org-id))))
    (let ((existing (assoc id identifiers :test #'string=)))
      (when (and existing (not (eq (cdr existing) heading)))
        (org-id-signal-error :duplicate-id
                             "Generated ID already occurs in the document")))
    (values (set-property document heading "ID" id) id)))

(defstruct (org-id-index (:constructor %make-org-id-index (entries))
                         (:conc-name %org-id-index-)
                         (:copier nil)
                         (:predicate %org-id-index-p))
  "Snapshot index mapping Org ID strings to matching headlines and inlinetasks."
  (entries (make-hash-table :test #'equal) :read-only t))

(defun make-org-id-index (documents)
  "Index ID properties across DOCUMENTS in document and target order.
DOCUMENTS may be one parsed or unparsed document, or a proper list of them.
Duplicate IDs are retained as separate candidates; invalid stored IDs are skipped."
  (let ((entries (make-hash-table :test #'equal)))
    (dolist (document (org-id-normalize-documents documents))
      (dolist (target (org-id-document-id-targets document))
        (let ((target-ids nil))
          (dolist (property (org-id-target-id-properties target))
            (let ((id (node-property property :value)))
              (when (org-id-valid-value-p id)
                (unless (member id target-ids :test #'string=)
                  (push id target-ids)
                  (push target (gethash id entries)))))))))
    (maphash (lambda (id candidates)
               (setf (gethash id entries) (nreverse candidates)))
             entries)
    (%make-org-id-index entries)))

(defun org-id-index-p (object)
  (%org-id-index-p object))

(defun org-id-index-candidates (index id)
  "Return matching headlines and inlinetasks for ID in document order.
ID must be non-empty, contain a character other than spaces or tabs, and be
single-line. The returned list is fresh."
  (unless (org-id-index-p index)
    (org-id-signal-error :invalid-index "Expected an Org ID index"))
  (org-id-check-value id)
  (copy-list (gethash id (%org-id-index-entries index))))
