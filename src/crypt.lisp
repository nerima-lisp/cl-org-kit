;;;; Format-preserving org-crypt transformations.
(in-package #:org-kit)

(defparameter *crypt-begin-marker* "-----BEGIN PGP MESSAGE-----")
(defparameter *crypt-end-marker* "-----END PGP MESSAGE-----")

(defun crypt-headlines (document)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               (document-root document))
    (nreverse headlines)))

(defun crypt-find-headline-at (document position)
  (find position (crypt-headlines document)
        :key #'node-post-affiliated :test #'=))

(defun crypt-line-bounds (source start limit)
  (let* ((newline (position #\Newline source :start start :end limit))
         (line-end (or newline limit))
         (content-end (if (and newline (> line-end start)
                               (char= (char source (1- line-end)) #\Return))
                          (1- line-end)
                          line-end)))
    (values content-end (if newline (1+ newline) limit))))

(defun crypt-horizontal-space-p (character)
  (find character '(#\Space #\Tab)))

(defun crypt-blank-line-p (line)
  (every #'crypt-horizontal-space-p line))

(defun crypt-trim-horizontal (line)
  (string-trim '(#\Space #\Tab) line))

(defun crypt-planning-line-p (line)
  (let ((line (string-left-trim '(#\Space #\Tab) line)))
    (some (lambda (prefix)
            (and (>= (length line) (length prefix))
                 (string-equal line prefix :end1 (length prefix))))
          '("DEADLINE:" "SCHEDULED:" "CLOSED:"))))

(defun crypt-clock-line-p (line)
  (let ((line (string-left-trim '(#\Space #\Tab) line)))
    (and (>= (length line) 6)
         (string-equal line "CLOCK:" :end1 6))))

(defun crypt-drawer-end (source start limit)
  (loop with cursor = start
        while (< cursor limit)
        do (multiple-value-bind (line-end next)
               (crypt-line-bounds source cursor limit)
             (when (string-equal (crypt-trim-horizontal
                                  (subseq source cursor line-end))
                                 ":END:")
               (return-from crypt-drawer-end next))
             (setf cursor next))
        finally (return nil)))

(defun crypt-leading-metadata-end (document heading)
  (let* ((source (document-text document))
         (headline-end (nth-value 1
                                  (crypt-line-bounds
                                   source (node-post-affiliated heading)
                                   (length source))))
         (position headline-end)
         (limit (length source)))
    (when (< position limit)
      (multiple-value-bind (line-end next)
          (crypt-line-bounds source position limit)
        (when (crypt-planning-line-p (subseq source position line-end))
          (setf position next))))
    (when (< position limit)
      (multiple-value-bind (line-end next)
          (crypt-line-bounds source position limit)
        (when (string-equal
               (crypt-trim-horizontal (subseq source position line-end))
               ":PROPERTIES:")
          (let ((drawer-end (crypt-drawer-end source next limit)))
            (when drawer-end
              (setf position drawer-end))))))
    (loop while (< position limit)
          do (when (crypt-find-headline-at document position)
               (return position))
             (multiple-value-bind (line-end next)
                 (crypt-line-bounds source position limit)
               (let ((line (subseq source position line-end)))
                 (cond ((crypt-blank-line-p line)
                        (setf position next))
                       ((string-equal (crypt-trim-horizontal line) ":LOGBOOK:")
                        (let ((drawer-end (crypt-drawer-end source next limit)))
                          (if drawer-end
                              (setf position drawer-end)
                              (return position))))
                       ((crypt-clock-line-p line)
                        (setf position next))
                       (t (return position)))))
          finally (return position))))

(defun crypt-subtree-end (document heading)
  (let* ((source (document-text document))
         (position (node-post-affiliated heading))
         (level (node-property heading :level))
         (next-heading
           (find-if (lambda (candidate)
                      (and (> (node-post-affiliated candidate) position)
                           (<= (node-property candidate :level) level)))
                    (crypt-headlines document)))
         (limit (if next-heading (node-begin next-heading) (length source)))
         (cursor position)
         (last-content-end position))
    (loop while (< cursor limit)
          do (multiple-value-bind (line-end next)
                 (crypt-line-bounds source cursor limit)
               (unless (crypt-blank-line-p
                        (subseq source cursor line-end))
                 (setf last-content-end line-end))
               (setf cursor next)))
    last-content-end))

(defun crypt-entry-payload-span (document heading)
  (let ((start (crypt-leading-metadata-end document heading))
        (end (crypt-subtree-end document heading)))
    (values start (max start end))))

(defun crypt-armor-line-kind (source start limit)
  (multiple-value-bind (line-end next)
      (crypt-line-bounds source start limit)
    (declare (ignore next))
    (let* ((line (subseq source start line-end))
           (indent-end (position-if-not #'crypt-horizontal-space-p line))
           (indent-end (or indent-end (length line)))
           (marker (subseq line indent-end)))
      (cond ((string= marker *crypt-begin-marker*) :begin)
            ((string= marker *crypt-end-marker*) :end)
            (t nil)))))

(defun crypt-complete-armor-p (armor)
  (let ((limit (length armor))
        (cursor 0)
        (line-count 0)
        (begin-count 0)
        (end-count 0)
        (first-kind nil)
        (last-kind nil))
    (loop while (< cursor limit)
          do (multiple-value-bind (line-end next)
                 (crypt-line-bounds armor cursor limit)
               (declare (ignore line-end))
               (let ((kind (crypt-armor-line-kind armor cursor limit)))
                 (when (zerop line-count)
                   (setf first-kind kind))
                 (when (eq kind :begin)
                   (incf begin-count))
                 (when (eq kind :end)
                   (incf end-count))
                 (setf last-kind kind
                       cursor next)
                 (incf line-count)))
          finally (return (and (eq first-kind :begin)
                               (eq last-kind :end)
                               (= begin-count 1)
                               (= end-count 1))))))

(defun crypt-headline-at-line-p (document position)
  (not (null (crypt-find-headline-at document position))))

(defun crypt-find-armor-span (document start end)
  (let ((source (document-text document)))
    (when (and (< start end)
               (eq (crypt-armor-line-kind source start end) :begin))
      (multiple-value-bind (first-line-end cursor)
          (crypt-line-bounds source start end)
        (declare (ignore first-line-end))
        (loop while (< cursor end)
              do (when (crypt-headline-at-line-p document cursor)
                   (return-from crypt-find-armor-span nil))
                 (multiple-value-bind (line-end next)
                     (crypt-line-bounds source cursor end)
                   (when (eq (crypt-armor-line-kind source cursor end) :end)
                     (return-from crypt-find-armor-span
                       (values start line-end)))
                   (setf cursor next))
              finally (return nil))))))

(defun crypt-remove-armor-indentation (source start end)
  (with-output-to-string (output)
    (loop with cursor = start
          while (< cursor end)
          do (multiple-value-bind (line-end next)
                 (crypt-line-bounds source cursor end)
               (let ((indent-end cursor))
                 (loop while (and (< indent-end line-end)
                                  (crypt-horizontal-space-p
                                   (char source indent-end)))
                       do (incf indent-end))
                 (write-string source output :start indent-end :end line-end)
                 (write-string source output :start line-end :end next))
               (setf cursor next)))))

(defun crypt-heading-line-level (line)
  (let ((length (length line)))
    (loop for position from 0 below length
          while (char= (char line position) #\*)
          finally (return
                    (and (plusp position)
                         (< position length)
                         (crypt-horizontal-space-p (char line position))
                         position)))))

(defun crypt-plaintext-heading-shift (plaintext heading-level)
  (let ((cursor 0)
        (minimum-level nil)
        (heading-count 0))
    (loop while (< cursor (length plaintext))
          do (multiple-value-bind (line-end next)
                 (crypt-line-bounds plaintext cursor (length plaintext))
               (let ((level (crypt-heading-line-level
                             (subseq plaintext cursor line-end))))
                 (when level
                   (incf heading-count)
                   (setf minimum-level (if minimum-level
                                           (min minimum-level level)
                                           level))))
               (setf cursor next)))
    (let ((shift (if (and minimum-level (<= minimum-level heading-level))
                     (1+ (- heading-level minimum-level))
                     0)))
      (values shift heading-count))))

(defun crypt-shift-plaintext-headings (document start end plaintext heading-level)
  (multiple-value-bind (shift heading-count)
      (crypt-plaintext-heading-shift plaintext heading-level)
    (when (plusp shift)
      (let* ((source (document-text document))
             (new-length (+ (- (length source) (- end start))
                            (length plaintext) (* shift heading-count))))
        (check-limit (document-limits document) :max-output-length new-length)
      (return-from crypt-shift-plaintext-headings
        (with-output-to-string (output)
          (loop with cursor = 0
                while (< cursor (length plaintext))
                do (multiple-value-bind (line-end next)
                       (crypt-line-bounds plaintext cursor (length plaintext))
                     (let* ((line (subseq plaintext cursor line-end))
                            (level (crypt-heading-line-level line)))
                       (when level
                         (loop repeat shift do (write-char #\* output)))
                       (write-string line output)
                       (write-string plaintext output
                                     :start line-end :end next))
                     (setf cursor next)))))))
    plaintext))

(defun crypt-signal-callback-failure (reason)
  (error 'org-crypt-error
         :message (if (eq reason :missing-callback)
                      "Org crypt callback is not registered"
                      "Org crypt callback failed")
         :reason reason))

(defun crypt-invoke (registry kind text)
  (let ((result
          (handler-case
              (invoke-callback registry kind "org-crypt" (copy-seq text))
            (org-callback-error (condition)
              (crypt-signal-callback-failure
               (if (eq (org-callback-error-reason condition) :unregistered)
                   :missing-callback
                   :callback-failed)))
            (error () (crypt-signal-callback-failure :callback-failed)))))
    (unless (stringp result)
      (error 'org-crypt-error
             :message "Org crypt callback returned a non-string value"
             :reason :invalid-callback-result))
    result))

(defun crypt-check-document-and-registry (document registry)
  (unless (document-p document)
    (error 'type-error :datum document :expected-type 'document))
  (unless (callback-registry-p registry)
    (error 'type-error :datum registry :expected-type 'callback-registry)))

(defun crypt-check-output-length (document start end replacement)
  (check-limit (document-limits document) :max-output-length
               (+ (- (length (document-text document)) (- end start))
                  (length replacement))))

(defun crypt-apply-patches-result (document patches)
  (let* ((prepared (edit-prepare-patches document patches))
         (updated-source
           (edit-splice-source (edit-prepared-patches-source prepared)
                               (edit-prepared-patches-patches prepared)))
         (updated-document (edit-reparse-source document updated-source nil)))
    (edit-apply-patches-result document patches
                               :expected
                               (edit-canonical-document updated-document))))

(defun crypt-no-op-result (document)
  (crypt-apply-patches-result document nil))

(defun crypt-transform-entry (document target registry direction)
  (crypt-check-document-and-registry document registry)
  (let* ((heading (edit-resolve-node document target '(:headline)))
         (source (document-text document)))
    (multiple-value-bind (body-start body-end)
        (crypt-entry-payload-span document heading)
      (multiple-value-bind (armor-start armor-end)
          (and (< body-start body-end)
               (crypt-find-armor-span document body-start body-end))
        (case direction
          (:encrypt
           (if (or (>= body-start body-end) armor-start)
               (crypt-no-op-result document)
               (let ((replacement (crypt-invoke
                                   registry :crypt-encrypt
                                   (subseq source body-start body-end))))
                 (crypt-check-output-length document body-start body-end replacement)
                 (unless (crypt-complete-armor-p replacement)
                   (error 'org-crypt-error
                          :message "Org crypt callback did not return one PGP message armor block"
                          :reason :invalid-callback-result))
                 (crypt-apply-patches-result
                  document
                  (list (edit-make-patch body-start body-end replacement heading))))))
          (:decrypt
           (if (null armor-start)
               (crypt-no-op-result document)
               (let* ((armor (crypt-remove-armor-indentation
                              source armor-start armor-end))
                      (plaintext (crypt-invoke registry :crypt-decrypt armor)))
                 (edit-prepare-patches
                  document
                  (list (edit-make-patch
                         armor-start armor-end plaintext heading))
                  :target heading)
                 (let ((replacement
                         (crypt-shift-plaintext-headings
                          document armor-start armor-end plaintext
                          (node-property heading :level))))
                   (crypt-check-output-length
                    document armor-start armor-end replacement)
                   (crypt-apply-patches-result
                    document
                    (list (edit-make-patch
                           armor-start armor-end replacement heading))))))))))))

(defun crypt-encrypt-entry (document target registry)
  "Encrypt TARGET's body with the caller-owned registry and return an edit result.
TARGET is a headline node. Existing armor and empty bodies produce an unchanged
edit result. This operation does not require a crypt tag."
  (crypt-transform-entry document target registry :encrypt))

(defun crypt-decrypt-entry (document target registry)
  "Decrypt TARGET's first PGP armor block and return an edit result.
The callback receives armor with line indentation removed. Text after the
footer remains in the document. This operation does not require a crypt tag."
  (crypt-transform-entry document target registry :decrypt))

(defun crypt-tag-name (tag)
  (typecase tag
    (string tag)
    (symbol (string-downcase (symbol-name tag)))
    (t (error 'type-error :datum tag :expected-type '(or string symbol)))))

(defun crypt-tagged-p (document heading tag inherit)
  (member tag (effective-heading-tags document heading :inherit inherit)
          :test #'string=))

(defun crypt-excluded-headline-p (document heading)
  (or (member "ARCHIVE"
              (effective-heading-tags document heading :inherit t)
              :test #'string=)
      (loop for ancestor = heading then (node-parent ancestor)
            while ancestor
            thereis (and (eq (node-type ancestor) :headline)
                         (node-property ancestor :commentedp)))))

(defun crypt-matching-headlines (document tag inherit)
  (remove-if-not (lambda (heading)
                   (and (crypt-tagged-p document heading tag inherit)
                        (not (crypt-excluded-headline-p document heading))))
                 (crypt-headlines document)))

(defun crypt-matching-roots (document tag inherit)
  (let ((matching (crypt-matching-headlines document tag inherit)))
    (remove-if (lambda (heading)
                 (loop for parent = (node-parent heading) then (node-parent parent)
                       while parent
                       thereis (and (eq (node-type parent) :headline)
                                    (member parent matching :test #'eq))))
               matching)))

(defun crypt-headlines-in-subtree (heading)
  (let ((headlines '()))
    (map-nodes (lambda (node)
                 (when (eq (node-type node) :headline)
                   (push node headlines)))
               heading)
    (nreverse headlines)))

(defun crypt-decrypt-root (document root registry)
  (let ((candidate
          (find-if
           (lambda (heading)
             (multiple-value-bind (body-start body-end)
                 (crypt-entry-payload-span document heading)
               (and (< body-start body-end)
                    (multiple-value-bind (armor-start armor-end)
                        (crypt-find-armor-span document body-start body-end)
                      (declare (ignore armor-end))
                      armor-start))))
           (crypt-headlines-in-subtree root))))
    (if candidate
        (values (edit-result-updated-document
                 (crypt-decrypt-entry document candidate registry))
                t)
        (values document nil))))

(defun crypt-bulk-transform (document registry direction tag inherit)
  (crypt-check-document-and-registry document registry)
  (unless (or (null inherit) (eq inherit t))
    (error 'type-error :datum inherit :expected-type 'boolean))
  (let* ((tag (crypt-tag-name tag))
         (roots (crypt-matching-roots document tag inherit))
         (patches '()))
    (dolist (root roots)
      (multiple-value-bind (updated changed-p)
          (ecase direction
            (:encrypt
             (let ((result (crypt-transform-entry document root registry :encrypt)))
               (values (edit-result-updated-document result)
                       (not (null (edit-result-spans result))))))
            (:decrypt (crypt-decrypt-root document root registry)))
        (when changed-p
          (multiple-value-bind (body-start body-end)
              (crypt-entry-payload-span document root)
            (let* ((updated-heading
                     (crypt-find-headline-at updated (node-post-affiliated root)))
                   (updated-end (crypt-subtree-end updated updated-heading))
                   (updated-source (document-text updated)))
              (push (edit-make-patch
                     body-start body-end
                     (subseq updated-source body-start updated-end) root)
                    patches))))))
    (crypt-apply-patches-result document (nreverse patches))))

(defun crypt-encrypt-entries (document registry &key (tag "crypt") (inherit t))
  "Encrypt tagged entries and return one atomic edit result.
TAG defaults to \"crypt\". INHERIT defaults to true, matching Org tag search."
  (crypt-bulk-transform document registry :encrypt tag inherit))

(defun crypt-decrypt-entries (document registry &key (tag "crypt") (inherit t))
  "Decrypt tagged entries and return one atomic edit result.
TAG defaults to \"crypt\". INHERIT defaults to true, matching Org tag search."
  (crypt-bulk-transform document registry :decrypt tag inherit))
