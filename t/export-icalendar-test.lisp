(in-package #:cl-org-kit/test)

(defun export-icalendar-test-path (relative)
  (asdf:system-relative-pathname
   "cl-org-kit"
   (concatenate 'string "t/fixtures/export/oracle/icalendar/" relative)))

(defun export-icalendar-test-read-text (relative)
  (uiop:read-file-string (export-icalendar-test-path relative)))

(defun export-icalendar-test-read-octets (relative)
  (with-open-file (stream (export-icalendar-test-path relative)
                          :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (unless (= (read-sequence octets stream) length)
        (error "Unable to read all iCalendar fixture bytes from ~A" relative))
      octets)))

(defun export-icalendar-test-tsv-fields (line)
  (loop with start = 0
        for tab = (position #\Tab line :start start)
        collect (subseq line start tab)
        while tab
          do (setf start (1+ tab))))

(defun export-icalendar-test-manifest-rows ()
  (mapcar #'export-icalendar-test-tsv-fields
          (rest (uiop:read-file-lines
                 (export-icalendar-test-path "manifest.tsv")))))

(defun export-icalendar-test-context ()
  (make-export-context
   :time-context
   (make-time-context
    :reference-time (make-org-time-value "2000-01-01T00:00:00+00:00")
    :zone (resolve-time-zone "UTC"))))

(defun export-icalendar-test-random-source ()
  (let ((octet 0)
        (calls 0))
    (values (lambda ()
              (prog1 (mod octet 256)
                (incf octet)
                (incf calls)))
            (lambda () calls))))

(defun export-icalendar-test-catch-error (function)
  (handler-case (progn (funcall function) nil)
    (error (condition) condition)))

(defun export-icalendar-test-metadata-value (metadata key)
  (let ((prefix (concatenate 'string key "=")))
    (loop for line in (uiop:split-string metadata :separator '(#\Newline))
          for text = (string-right-trim '(#\Return) line)
          when (and (>= (length text) (length prefix))
                    (string= text prefix :end1 (length prefix)))
            return (subseq text (length prefix)))))

(defun export-icalendar-test-count (needle text)
  (loop with start = 0
        for found = (search needle text :start2 start)
        while found
          count 1
          do (setf start (+ found (length needle)))))

(defun export-icalendar-test-physical-lines (octets)
  (let ((start 0)
        (lines nil))
    (loop for index from 0 below (length octets)
          when (= (aref octets index) #x0d)
            do (unless (and (< (1+ index) (length octets))
                            (= (aref octets (1+ index)) #x0a))
                 (error "iCalendar output contains a non-CRLF line ending"))
               (push (subseq octets start index) lines)
               (setf start (+ index 2))
               (incf index))
    (when (< start (length octets))
      (push (subseq octets start) lines))
    (nreverse lines)))

(defun export-icalendar-test-line-string (octets)
  (cl-codec-kit:octets-to-string octets :encoding :utf-8))

(defun export-icalendar-test-metadata-path (expected)
  (concatenate 'string (subseq expected 0 (- (length expected) 4)) ".meta"))

(describe "FR-100 iCalendar export"
          (it "matches the pinned Org 9.8.10 fixtures byte for byte"
              (let ((rows (export-icalendar-test-manifest-rows))
                    (case-names nil))
                (with-soft-assertions
                 (expect (first (uiop:read-file-lines
                                 (export-icalendar-test-path "manifest.tsv")))
                         :to-equal (format nil "case~Csource~Cexpected" #\Tab #\Tab))
                 (expect rows
                         :to-equal
                         '(("plain-active" "source/plain-active.org" "expected/plain-active.ics")
                           ("scheduled-deadline" "source/scheduled-deadline.org" "expected/scheduled-deadline.ics")
                           ("todo-selection" "source/todo-selection.org" "expected/todo-selection.ics")
                           ("todo-all" "source/todo-all.org" "expected/todo-all.ics")
                           ("repeater" "source/repeater.org" "expected/repeater.ics")
                           ("timezones" "source/timezones.org" "expected/timezones.ics")
                           ("alarm" "source/alarm.org" "expected/alarm.ics")
                           ("escaping" "source/escaping.org" "expected/escaping.ics")
                           ("ascii-fold" "source/ascii-fold.org" "expected/ascii-fold.ics")
                           ("inbuffer-overrides" "source/inbuffer-overrides.org" "expected/inbuffer-overrides.ics")
                           ("todo-unblocked" "source/todo-unblocked.org" "expected/todo-unblocked.ics")
                           ("recurring-deadline-start" "source/recurring-deadline-start.org" "expected/recurring-deadline-start.ics")
                           ("date-time-format" "source/date-time-format.org" "expected/date-time-format.ics")
                           ("diary-block" "source/diary-block.org" "expected/diary-block.ics")
                           ("point-date-only" "source/point-date-only.org" "expected/point-date-only.ics")
                           ("point-date-only-utc" "source/point-date-only-utc.org" "expected/point-date-only-utc.ics")))
                 (dolist (row rows)
                   (destructuring-bind (case source expected) row
                     (push case case-names)
                     (let* ((source-text (export-icalendar-test-read-text source))
                            (expected-octets (export-icalendar-test-read-octets expected))
                            (metadata
                             (export-icalendar-test-read-text
                              (export-icalendar-test-metadata-path expected)))
                            (document (parse-org source-text :source-name source))
                            (random-values (multiple-value-list
                                            (export-icalendar-test-random-source)))
                            (random-source (first random-values))
                            (random-calls (second random-values))
                            (actual
                             (export-icalendar-documents
                              (list document)
                              :context (export-icalendar-test-context)
                              :random-source random-source
                              :output-type :octets))
                            (physical-lines
                             (export-icalendar-test-physical-lines actual)))
                       (expect (plusp (length source-text)) :to-be t)
                       (expect (plusp (length expected-octets)) :to-be t)
                       (expect (plusp (length metadata)) :to-be t)
                       (progn
                         (expect actual :to-equalp expected-octets)
                         (progn
                           (when (string= case "point-date-only")
                             (expect
                              (search "DTEND;VALUE=DATE:20260308"
                                      (export-icalendar-test-line-string actual))
                              :to-be-truthy))
                           (when (string= case "point-date-only-utc")
                             (expect
                              (search "DTSTART:20260307T000000Z"
                                      (export-icalendar-test-line-string actual))
                              :to-be-truthy)
                             (expect
                              (search "DTEND:20260308T000000Z"
                                      (export-icalendar-test-line-string actual))
                              :to-be-truthy))))
                       (expect (every (lambda (line) (<= (length line) 75))
                                      physical-lines)
                               :to-be t)
                       (expect (export-icalendar-test-metadata-value metadata "case")
                               :to-equal case)
                       (expect (export-icalendar-test-metadata-value metadata "source")
                               :to-equal source)
                       (expect (export-icalendar-test-metadata-value metadata "org")
                               :to-equal "9.8.10")
                       (expect (export-icalendar-test-metadata-value metadata "fixed-time")
                               :to-equal "2000-01-01T00:00:00Z")
                       (expect (export-icalendar-test-metadata-value metadata "dtstamp")
                               :to-equal "20000101T000000Z")
                       (expect (export-icalendar-test-metadata-value metadata "timezone")
                               :to-equal "UTC")
                       (expect (export-icalendar-test-metadata-value metadata "uid-source")
                               :to-equal "deterministic octet sequence 00 through 0f")
                       (expect (export-icalendar-test-metadata-value metadata "uid-octets")
                               :to-equal "00,01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f")
                       (expect (export-icalendar-test-metadata-value
                                metadata "uid-fallback-calls")
                               :to-equal
                               (if (member case '("plain-active" "diary-block" "point-date-only" "point-date-only-utc") :test #'string=)
                                   "1"
                                 "0"))
                       (expect (funcall random-calls)
                               :to-be
                               (if (member case '("plain-active" "diary-block" "point-date-only" "point-date-only-utc") :test #'string=)
                                   16
                                 0)))))
                 (expect (nreverse case-names)
                         :to-equal
                         '("plain-active" "scheduled-deadline" "todo-selection"
                                          "todo-all" "repeater" "timezones" "alarm" "escaping"
                                          "ascii-fold" "inbuffer-overrides" "todo-unblocked"
                                          "recurring-deadline-start" "date-time-format" "diary-block"
                                          "point-date-only" "point-date-only-utc")))))

          (it "keeps the existing export-org call shape and combines parsed documents"
              (let* ((context (export-icalendar-test-context))
                     (first (parse-org
                             (format nil "* First event~%:PROPERTIES:~%:ID: first~%:END:~%<2024-03-14 Thu 09:00-10:00>~%")
                             :source-name "first.org"))
                     (second (parse-org
                              (format nil "* Second event~%:PROPERTIES:~%:ID: second~%:END:~%<2024-03-15 Fri 09:00-10:00>~%")
                              :source-name "second.org"))
                     (single (export-org first :icalendar :context context))
                     (combined
                      (export-icalendar-documents (list first second) :context context)))
                (with-soft-assertions
                 (expect (search "BEGIN:VCALENDAR" single) :to-be-truthy)
                 (expect (search "UID:TS1-first" single) :to-be-truthy)
                 (expect (search "DTSTAMP:20000101T000000Z" single) :to-be-truthy)
                 (expect (export-icalendar-test-count "BEGIN:VCALENDAR" combined) :to-be 1)
                 (expect (export-icalendar-test-count "END:VCALENDAR" combined) :to-be 1)
                 (expect (search "SUMMARY:First event" combined) :to-be-truthy)
                 (expect (search "SUMMARY:Second event" combined) :to-be-truthy))))

          (it "uses explicit context extensions for export-org UID fallback"
              (let* ((document
                      (parse-org
                       (format nil "* No ID~%<2024-03-14 Thu 09:00-10:00>~%")))
                     (random-values (multiple-value-list
                                     (export-icalendar-test-random-source)))
                     (random-source (first random-values))
                     (random-calls (second random-values))
                     (context
                      (make-export-context
                       :time-context (export-context-time-context (export-icalendar-test-context))
                       :extensions (list (cons :icalendar-random-source random-source))))
                     (output (export-org document :icalendar :context context)))
                (with-soft-assertions
                 (expect (search "UID:TS1-00010203-0405-4607-8809-0a0b0c0d0e0f" output)
                         :to-be-truthy)
                 (expect (funcall random-calls) :to-be 16))))

          (it "applies keyword, call, and heading-property overrides in that order"
              (let* ((source
                      (concatenate
                       'string
                       "#+ICALENDAR_INCLUDE_BODY: nil" (format nil "~%~%")
                       (format nil
                               "* Property wins~%:PROPERTIES:~%:ID: property~%:ICALENDAR_INCLUDE_BODY: t~%:END:~%Property body remains.~%<2024-03-14 Thu 09:00-10:00>~%~%")
                       (format nil
                               "* Keyword wins over default~%:PROPERTIES:~%:ID: keyword~%:END:~%Keyword body is omitted.~%<2024-03-15 Fri 09:00-10:00>~%")))
                     (document (parse-org source))
                     (context (export-icalendar-test-context))
                     (keyword-output
                      (export-icalendar-documents (list document) :context context))
                     (call-output
                      (export-icalendar-documents
                       (list document) :context context :options '(:include-body 5))))
                (with-soft-assertions
                 (expect (search "DESCRIPTION:Property body remains." keyword-output)
                         :to-be-truthy)
                 (expect (search "Keyword body is omitted." keyword-output) :to-be nil)
                 (expect (search "DESCRIPTION:Property body remains." call-output)
                         :to-be-truthy)
                 (expect (search "DESCRIPTION:Keywo" call-output) :to-be-truthy))))

          (it "enforces 75 UTF-8 octets per physical line without splitting characters"
              (let* ((title (concatenate 'string (make-string 66 :initial-element #\a) "é"))
                     (source (format nil "* ~A~%:PROPERTIES:~%:ID: utf8-fold~%:END:~%<2024-03-14 Thu 09:00-10:00>~%" title))
                     (document (parse-org source))
                     (octets (export-icalendar-documents
                              (list document)
                              :context (export-icalendar-test-context)
                              :output-type :octets))
                     (lines (export-icalendar-test-physical-lines octets))
                     (strings (mapcar #'export-icalendar-test-line-string lines))
                     (summary-position
                      (position-if (lambda (line)
                                     (and (>= (length line) 8)
                                          (string= line "SUMMARY:" :end1 8)))
                                   strings)))
                (with-soft-assertions
                 (expect (every (lambda (line) (<= (length line) 75)) lines) :to-be t)
                 (expect (and summary-position (< (1+ summary-position) (length strings)))
                         :to-be t)
                 (when (and summary-position (< (1+ summary-position) (length strings)))
                   (expect (char= (char (nth (1+ summary-position) strings) 0) #\Space)
                           :to-be t)
                   (expect (search " é" (nth (1+ summary-position) strings))
                           :to-be-truthy))
                 (expect (every (lambda (line)
                                  (handler-case
                                      (progn (export-icalendar-test-line-string line) t)
                                    (error () nil)))
                                lines)
                         :to-be t))))

          (it "signals named time, random-source, malformed-input, and limit conditions"
              (let* ((with-id
                      (parse-org
                       (format nil "* Event~%:PROPERTIES:~%:ID: stable~%:END:~%<2024-03-14 Thu 09:00>~%")))
                     (without-id
                      (parse-org (format nil "* Event~%<2024-03-14 Thu 09:00>~%")))
                     (missing-time
                      (export-icalendar-test-catch-error
                       (lambda () (export-icalendar-documents (list with-id)))))
                     (missing-random
                      (export-icalendar-test-catch-error
                       (lambda ()
                         (export-icalendar-documents
                          (list without-id) :context (export-icalendar-test-context)))))
                     (malformed
                      (export-icalendar-test-catch-error
                       (lambda ()
                         (export-icalendar-documents (cons with-id :improper)))))
                     (limited
                      (export-icalendar-test-catch-error
                       (lambda ()
                         (export-icalendar-documents
                          (list (parse-org "Body" :limits (make-limits :max-node-count 1)))))))
                     (below-character-limit
                      (export-icalendar-test-catch-error
                       (lambda ()
                         (export-icalendar-documents
                          (list (parse-org "Body" :limits (make-limits :max-output-length 0)))
                          :context (export-icalendar-test-context)))))
                     (below-octet-limit
                      (export-icalendar-test-catch-error
                       (lambda ()
                         (export-icalendar-documents
                          (list (parse-org "Body" :limits (make-limits :max-output-octets 0)))
                          :context (export-icalendar-test-context)))))
                     (combined-limit
                      (export-icalendar-test-catch-error
                       (lambda ()
                         (export-icalendar-documents
                          (list (parse-org "Body" :limits (make-limits :max-output-length 1000))
                                (parse-org "Body" :limits (make-limits :max-output-length 32)))
                          :context (export-icalendar-test-context))))))
                (with-soft-assertions
                 (expect (typep missing-time 'org-time-error) :to-be t)
                 (when (typep missing-time 'org-time-error)
                   (expect (org-time-error-reason missing-time) :to-be :invalid-time))
                 (expect (typep missing-random 'org-export-error) :to-be t)
                 (when (typep missing-random 'org-export-error)
                   (expect (org-export-error-backend missing-random) :to-be :icalendar)
                   (expect (org-export-error-reason missing-random)
                           :to-be :missing-random-source))
                 (expect (typep malformed 'org-export-error) :to-be t)
                 (when (typep malformed 'org-export-error)
                   (expect (org-export-error-reason malformed) :to-be :invalid-documents))
                 (expect (typep limited 'org-resource-limit-error) :to-be t)
                 (when (typep limited 'org-resource-limit-error)
                   (expect (org-resource-limit-error-limit-name limited)
                           :to-be :max-node-count))
                 (expect (typep below-character-limit 'org-resource-limit-error) :to-be t)
                 (when (typep below-character-limit 'org-resource-limit-error)
                   (expect (org-resource-limit-error-limit-name below-character-limit)
                           :to-be :max-output-length))
                 (expect (typep below-octet-limit 'org-resource-limit-error) :to-be t)
                 (when (typep below-octet-limit 'org-resource-limit-error)
                   (expect (org-resource-limit-error-limit-name below-octet-limit)
                           :to-be :max-output-octets))
                 (expect (typep combined-limit 'org-resource-limit-error) :to-be t)
                 (when (typep combined-limit 'org-resource-limit-error)
                   (expect (org-resource-limit-error-limit-name combined-limit)
                           :to-be :max-output-length)))))

  (it "exports unblocked TODOs, recurring deadline starts, custom date formats, and UTC all-day dates"
      (let* ((context (export-icalendar-test-context))
             (unblocked
              (export-icalendar-documents
               (list (parse-org
                      (export-icalendar-test-read-text
                       "source/todo-unblocked.org")))
               :context context))
             (recurring
              (export-icalendar-documents
               (list (parse-org
                      (export-icalendar-test-read-text
                       "source/recurring-deadline-start.org")))
               :context context))
             (custom-format
              (export-icalendar-documents
               (list (parse-org
                      (export-icalendar-test-read-text
                       "source/date-time-format.org")))
               :context context))
             (current-start
              (export-icalendar-documents
               (list (parse-org
                      (format nil
                              "#+TODO: TODO | DONE~%* TODO Start at context time~%:PROPERTIES:~%:ID: current-start~%:END:~%")))
               :context context
               :options '(:include-todo t
                                        :todo-unscheduled-start :current-datetime))))
        (with-soft-assertions
         (expect (export-icalendar-test-count "BEGIN:VTODO" unblocked) :to-be 3)
         (expect (search "SUMMARY:Available child" unblocked) :to-be-truthy)
         (expect (search "SUMMARY:First ordered step" unblocked) :to-be-truthy)
         (expect (search "SUMMARY:Independent task" unblocked) :to-be-truthy)
         (expect (search "SUMMARY:Parent blocked by child" unblocked) :to-be nil)
         (expect (search "SUMMARY:Second ordered step" unblocked) :to-be nil)
         (expect (search "DTSTART;VALUE=DATE:20240329" recurring) :to-be-truthy)
         (expect (search "DUE;VALUE=DATE:20240412" recurring) :to-be-truthy)
         (expect (search "RRULE:FREQ=WEEKLY;INTERVAL=1" recurring) :to-be-truthy)
         (expect (search "DTSTART;TZID=Europe/Paris:20240401T093000" custom-format)
                 :to-be-truthy)
         (expect (search "DTEND;TZID=Europe/Paris:20240401T103000" custom-format)
                 :to-be-truthy)
         (expect (search "DTSTART:20000101T000000" current-start) :to-be-truthy)
         (progn
           (expect (search "DUE:" current-start) :to-be nil)
           (let* ((new-york-context
                   (make-time-context
                    :reference-time (make-org-time-value "2026-03-08T12:00:00")
                    :zone (resolve-time-zone "America/New_York")))
                  (new-york-output
                   (export-icalendar-documents
                    (list (parse-org
                           (format nil
                                   "* New York all-day event~%:PROPERTIES:~%:ID: new-york-all-day~%:TIMEZONE: UTC~%:END:~%<2026-03-08 Sun>~%")))
                    :context (make-export-context :time-context new-york-context))))
             (expect (search "DTSTART:20260308T050000Z" new-york-output)
                     :to-be-truthy)
             (expect (search "DTEND:20260309T040000Z" new-york-output)
                     :to-be-truthy))))))

  (it "signals named malformed-option, invalid-zone, and unsupported-option conditions"
      (let* ((document
              (parse-org
               (format nil
                       "* Event~%:PROPERTIES:~%:ID: stable~%:END:~%<2024-03-14 Thu 09:00>~%")))
             (context (export-icalendar-test-context))
             (malformed
              (export-icalendar-test-catch-error
               (lambda ()
                 (export-icalendar-documents
                  (list (parse-org
                         (format nil
                                 "#+ICALENDAR_ALARM_TIME: nope~%* Event~%<2024-03-14 Thu 09:00>~%")))
                  :context context))))
             (bad-format
              (export-icalendar-test-catch-error
               (lambda ()
                 (export-icalendar-documents
                  (list document)
                  :context context
                  :options '(:date-time-format ":%Q")))))
             (bad-zone
              (export-icalendar-test-catch-error
               (lambda ()
                 (export-icalendar-documents
                  (list document)
                  :context context
                  :options (list :timezone (format nil "UTC~%BEGIN:VEVENT"))))))
             (bad-ttl
              (export-icalendar-test-catch-error
               (lambda ()
                 (export-icalendar-documents
                  (list document)
                  :context context
                  :options (list :ttl (format nil "PT1H~%X-Injected: yes"))))))
             (bad-store-uid
              (export-icalendar-test-catch-error
               (lambda ()
                 (export-icalendar-documents
                  (list document)
                  :context context
                  :options '(:store-uid t)))))
             (bad-property-store-uid
              (export-icalendar-test-catch-error
               (lambda ()
                 (export-icalendar-documents
                  (list (parse-org
                         (format nil
                                 "* Event~%:PROPERTIES:~%:ID: stable~%:ICALENDAR_STORE_UID: t~%:END:~%<2024-03-14 Thu 09:00>~%")))
                  :context context)))))
        (with-soft-assertions
         (expect (typep malformed 'org-export-error) :to-be t)
         (when (typep malformed 'org-export-error)
           (expect (org-export-error-backend malformed) :to-be :icalendar)
           (expect (org-export-error-reason malformed) :to-be :invalid-option))
         (expect (typep bad-format 'org-export-error) :to-be t)
         (when (typep bad-format 'org-export-error)
           (expect (org-export-error-reason bad-format) :to-be :invalid-option))
         (expect (typep bad-zone 'org-export-error) :to-be t)
         (when (typep bad-zone 'org-export-error)
           (expect (org-export-error-reason bad-zone) :to-be :invalid-timezone))
         (expect (typep bad-ttl 'org-export-error) :to-be t)
         (when (typep bad-ttl 'org-export-error)
           (expect (org-export-error-reason bad-ttl) :to-be :invalid-option))
         (progn
           (dolist (condition (list bad-store-uid bad-property-store-uid))
             (expect (typep condition 'org-export-error) :to-be t)
             (when (typep condition 'org-export-error)
               (expect (org-export-error-backend condition) :to-be :icalendar)
               (expect (org-export-error-reason condition) :to-be :unsupported-option)))
           (let* ((unknown-bind
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list (parse-org
                              (format nil
                                      "#+BIND: org-icalendar-unknown-option t~%* Event~%<2024-03-14 Thu 09:00>~%")))
                       :context context))))
                  (unknown-selector
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list document)
                       :context context
                       :options '(:use-scheduled "unknown-selector")))))
                  (unknown-mode
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list document)
                       :context context
                       :options '(:with-timestamps "unknown_mode")))))
                  (wrong-timestamp-mode
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list document)
                       :context context
                       :options '(:with-timestamps :current-datetime)))))
                  (wrong-unscheduled-start
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list document)
                       :context context
                       :options '(:todo-unscheduled-start :active)))))
                  (wrong-deadline-selector
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list document)
                       :context context
                       :options '(:use-deadline (:todo-start))))))
                  (wrong-scheduled-selector
                   (export-icalendar-test-catch-error
                    (lambda ()
                      (export-icalendar-documents
                       (list document)
                       :context context
                       :options '(:use-scheduled (:todo-due))))))
                  (recognized-bind-output
                   (export-icalendar-documents
                    (list (parse-org
                           (format nil
                                   "#+TODO: TODO | DONE~%#+BIND: org-icalendar-include-todo t~%* TODO Bound by BIND~%:PROPERTIES:~%:ID: bind-id~%:END:~%")))
                    :context context)))
             (dolist (condition
                      (list unknown-bind unknown-selector unknown-mode
                            wrong-timestamp-mode wrong-unscheduled-start
                            wrong-deadline-selector wrong-scheduled-selector))
               (expect (typep condition 'org-export-error) :to-be t)
               (when (typep condition 'org-export-error)
                 (expect (org-export-error-backend condition) :to-be :icalendar)
                 (expect (org-export-error-reason condition) :to-be :invalid-option)))
             (expect (export-icalendar-test-count "BEGIN:VTODO" recognized-bind-output)
                     :to-be 1))))))

  (it "rejects diary sexps without evaluation and supports the safe diary-block subset"
    (let* ((document
             (parse-org
              (format nil
                      "* Event~%:PROPERTIES:~%:ID: diary-event~%:END:~%<2026-12-25 Fri 09:00>~%%%(diary-date 12 25 2026)~%")))
           (safe-document
             (parse-org
              (format nil
                      "* Literal range~%:PROPERTIES:~%:ID: static-range~%:END:~%<%%(diary-block 1 2 2025 1 4 2025)>~%")))
           (root-document
             (parse-org "%%(diary-block 1 2 2025 1 4 2025) Root block"))
           (unsupported
             (export-icalendar-test-catch-error
              (lambda ()
                (export-icalendar-documents
                 (list document) :context (export-icalendar-test-context)))))
           (omitted
             (export-icalendar-documents
              (list document)
              :context (export-icalendar-test-context)
              :options '(:include-sexps nil)))
           (supported
             (export-icalendar-documents
              (list safe-document)
              :context (export-icalendar-test-context)))
           (random-values
             (multiple-value-list (export-icalendar-test-random-source)))
           (root-random-source (first random-values))
           (root-random-calls (second random-values))
           (root-output
             (export-icalendar-documents
              (list root-document)
              :context (export-icalendar-test-context)
              :random-source root-random-source)))
      (with-soft-assertions
        (expect (typep unsupported 'org-export-unsupported-syntax) :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend unsupported)
                  :to-be :icalendar)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp))
        (expect (search "%%(diary-date" omitted) :to-be nil)
        (expect (search "UID:DS1-static-range" supported) :to-be-truthy)
        (expect (search "DTSTAMP:20000101T000000Z" supported) :to-be-truthy)
        (expect (search "RRULE:FREQ=DAILY;UNTIL=20250104" supported)
                :to-be-truthy)
        (expect (search "DTSTART;VALUE=DATE:20250102" supported)
                :to-be-truthy)
        (expect (search "SUMMARY:Root block" root-output) :to-be-truthy)
        (expect (search "UID:00010203-0405-4607-8809-0a0b0c0d0e0f"
                        root-output)
                :to-be-truthy)
        (expect (funcall root-random-calls) :to-be 16)))))

(describe "iCalendar coverage paths"
  (it "covers UTF-8 widths, escaped line breaks, folding, and malformed fragments"
      (let* ((builder (org-kit::%make-icalendar-builder :limits (make-limits)))
             (bad-fragment
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-append-line builder (list (cons 42 nil))))))
             (bad-surrogate
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-utf8-width (code-char #xd800))))))
        (with-soft-assertions
          (expect (org-kit::%icalendar-utf8-width #\A) :to-be 1)
          (expect (org-kit::%icalendar-utf8-width (code-char #x80)) :to-be 2)
          (expect (org-kit::%icalendar-utf8-width (code-char #x800)) :to-be 3)
          (expect (org-kit::%icalendar-utf8-width (code-char #x1f600)) :to-be 4)
          (expect (typep bad-surrogate 'org-export-error) :to-be t)
          (expect (org-export-error-reason bad-surrogate) :to-be :encoding)
          (expect (org-kit::%icalendar-text-character-width #\; t) :to-be 2)
          (expect (org-kit::%icalendar-text-character-width #\a nil) :to-be 1)
          (org-kit::%icalendar-append-escaped-newline builder 73)
          (org-kit::%icalendar-append-escaped-newline builder 74)
          (org-kit::%icalendar-append-line
           builder
           (list (cons "X:" nil)
                 (cons (make-string 74 :initial-element #\a) t)))
          (org-kit::%icalendar-append-line
           builder
           (list (cons "TEXT:" nil)
                 (cons (format nil "a~C~Cb~Cc~Cd" #\Return #\Newline
                               #\Return #\Newline)
                       t)))
          (org-kit::%icalendar-append-line
           builder
           (list (cons "ESCAPE:" nil) (cons "\\;," t)))
          (expect (typep bad-fragment 'type-error) :to-be t)
          (let ((text (coerce (org-kit::%icalendar-builder-buffer builder)
                              'string)))
            (expect (search (format nil "~C~C " #\Return #\Newline) text)
                    :to-be-truthy)
            (expect (search "\\n" text) :to-be-truthy)
            (expect (search "\\\\\\;\\," text) :to-be-truthy)))))

  (it "covers option values and finite BIND parsing"
      (let* ((bad-boolean
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-parse-option-value :force-alarm 1))))
             (bad-todo-item
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value :include-todo '(3)))))
             (bad-todo
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value :include-todo 3))))
             (bad-body
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-parse-option-value :include-body "-2"))))
             (bad-body-type
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-parse-option-value :include-body 2.5))))
             (bad-integer
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-parse-option-value :alarm-time "x"))))
             (bad-list-type
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-parse-option-value :categories 3))))
             (bad-selector
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-list-option-item :use-scheduled "nope"))))
             (bad-category
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-list-option-item :categories "nope"))))
             (bad-category-item
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-list-option-item :categories 3))))
             (bad-mode
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value :with-timestamps "nope"))))
             (bad-mode-type
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value :with-timestamps 4))))
             (bad-string
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-parse-option-value :description nil))))
             (bad-ttl
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value
                   :ttl (format nil "PT1H~%X")))))
             (bad-options-shape
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-explicit-options '(:alarm-time)))))
             (bad-options-type
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-explicit-options 3))))
             (bad-options-key
               (export-icalendar-test-catch-error
                (lambda () (org-kit::%icalendar-explicit-options '(:unknown t)))))
             (duplicate-keyword
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-keyword-options
                   (parse-org
                    (format nil
                            "#+ICALENDAR_ALARM_TIME: 1~%#+ICALENDAR_ALARM_TIME: 2~%"))))))
             (bad-bind-key
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-bind-option "org-icalendar-unknown t"))))
             (bad-bind-list
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-bind-option
                   "org-icalendar-categories (local-tags"))))
             (bad-bind-string
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-bind-option
                   "org-icalendar-description \"unterminated"))))
             (explicit
               (org-kit::%icalendar-explicit-options
                '(:alarm-time 2 :alarm-time 4))))
        (with-soft-assertions
          (expect (org-kit::%icalendar-keyword-option-name "ICAL-TTL") :to-be :ttl)
          (expect (org-kit::%icalendar-keyword-option-name "ICALENDAR_ALARM_TIME")
                  :to-be :alarm-time)
          (expect (org-kit::%icalendar-keyword-option-name :not-a-string) :to-be nil)
          (expect (org-kit::%icalendar-keyword-option-name "OTHER") :to-be nil)
          (expect (org-kit::%icalendar-parse-option-value :force-alarm "t") :to-be t)
          (expect (org-kit::%icalendar-parse-option-value :force-alarm "nil") :to-be nil)
          (expect (org-kit::%icalendar-parse-option-value :include-todo :all) :to-be :all)
          (expect (org-kit::%icalendar-parse-option-value :include-todo "all") :to-be :all)
          (expect (org-kit::%icalendar-parse-option-value :include-todo "unblocked")
                  :to-be :unblocked)
          (expect (org-kit::%icalendar-parse-option-value :include-todo "TODO WAIT")
                  :to-equal '("TODO" "WAIT"))
          (expect (org-kit::%icalendar-parse-option-value :include-todo '(:todo "WAIT"))
                  :to-equal '("TODO" "WAIT"))
          (expect (org-kit::%icalendar-parse-option-value :include-body 0) :to-be 0)
          (expect (org-kit::%icalendar-parse-option-value :include-body "3") :to-be 3)
          (expect (org-kit::%icalendar-parse-option-value :alarm-time "0") :to-be 0)
          (expect (org-kit::%icalendar-parse-option-value :use-scheduled
                                                          "todo-start event-if-todo")
                  :to-equal '(:todo-start :event-if-todo))
          (expect (org-kit::%icalendar-parse-option-value :categories
                                                          '(:all-tags :todo-state))
                  :to-equal '("all-tags" "todo-state"))
          (expect (org-kit::%icalendar-parse-option-value :with-timestamps t)
                  :to-be :all)
          (expect (org-kit::%icalendar-parse-option-value :with-timestamps "inactive")
                  :to-be :inactive)
          (expect (org-kit::%icalendar-parse-option-value
                   :todo-unscheduled-start "nil") :to-be nil)
          (expect (org-kit::%icalendar-parse-option-value :description "text")
                  :to-equal "text")
          (expect (cdr (assoc :alarm-time explicit)) :to-be 4)
          (expect (typep bad-boolean 'org-export-error) :to-be t)
          (expect (typep bad-todo-item 'type-error) :to-be t)
          (expect (typep bad-todo 'org-export-error) :to-be t)
          (expect (typep bad-body 'org-export-error) :to-be t)
          (expect (typep bad-body-type 'org-export-error) :to-be t)
          (expect (typep bad-integer 'org-export-error) :to-be t)
          (expect (typep bad-list-type 'org-export-error) :to-be t)
          (expect (typep bad-selector 'org-export-error) :to-be t)
          (expect (typep bad-category 'org-export-error) :to-be t)
          (expect (typep bad-category-item 'type-error) :to-be t)
          (expect (typep bad-mode 'org-export-error) :to-be t)
          (expect (typep bad-mode-type 'org-export-error) :to-be t)
          (expect (typep bad-string 'type-error) :to-be t)
          (expect (typep bad-ttl 'org-export-error) :to-be t)
          (expect (typep bad-options-shape 'type-error) :to-be t)
          (expect (typep bad-options-type 'type-error) :to-be t)
          (expect (typep bad-options-key 'org-export-error) :to-be t)
          (expect (typep duplicate-keyword 'org-export-unsupported-syntax) :to-be t)
          (expect (typep bad-bind-key 'org-export-error) :to-be t)
          (expect (typep bad-bind-list 'org-export-error) :to-be t)
          (expect (typep bad-bind-string 'org-export-error) :to-be t)
          (expect (multiple-value-list
                   (org-kit::%icalendar-bind-option
                    "org-icalendar-categories (local-tags category)"))
                  :to-equal '(:categories ("local-tags" "category")))
          (expect (multiple-value-list
                   (org-kit::%icalendar-bind-option
                    "org-icalendar-include-todo t"))
                  :to-equal '(:include-todo "t"))))))

(describe "iCalendar node and event helper coverage"
  (it "covers headline titles, node text variants, body normalization, filling, and limits"
      (let* ((plain (make-node :plain-text :properties '(:value "plain")))
             (empty-plain (make-node :plain-text))
             (entity-utf8 (make-node :entity :properties '(:utf-8 "utf8")))
             (entity-unicode (make-node :entity :properties '(:unicode "unicode")))
             (line-break (make-node :line-break))
             (inactive (make-node :timestamp :properties '(:type :inactive :value "[date]")))
             (active (make-node :timestamp :properties '(:type :active :value "<date>")))
             (path-link (make-node :link :properties '(:path "target")))
             (text-link (make-node :link :children (list plain)))
             (ignored (make-node :drawer :properties '(:value "ignored")))
             (generic-child (make-node :paragraph :children (list plain)))
             (generic-value (make-node :paragraph :properties '(:value "fallback")))
             (fallback-heading (make-node :headline :properties '(:raw-value "raw")))
             (empty-heading (make-node :headline))
             (title-heading (make-node :headline
                                       :properties (list :title (list "prefix" plain))))
             (document (make-document "E"))
             (attached-entity (make-node :entity :begin 0 :end 1))
             (root (make-node :org-data :children (list attached-entity)))
             (body-text (format nil "a~C~Cb~Cc~Cd" #\Return #\Newline
                                #\Return #\Newline))
             (paragraph (make-node :paragraph :properties (list :value body-text)))
             (section (make-node :section :begin 0 :end (length body-text)
                                 :children (list paragraph)))
             (body-heading (make-node :headline :properties '(:level 1)
                                      :children (list section)))
             (body (org-kit::%icalendar-headline-body
                    body-heading '((:include-body . t)) (make-limits)))
             (short-body (org-kit::%icalendar-headline-body
                          body-heading '((:include-body . 3)) (make-limits)))
             (wrapped-section
               (make-node :section
                          :children
                          (list (make-node
                                 :paragraph
                                 :properties
                                 (list :value
                                       (concatenate 'string
                                                    (make-string 70 :initial-element #\a)
                                                    " bb"))))))
             (wrapped-heading (make-node :headline :properties '(:level 4)
                                         :children (list wrapped-section)))
             (unbounded-section
               (make-node :section :children (list (make-node :paragraph
                                                              :properties
                                                              '(:value "body")))))
             (unbounded-heading (make-node :headline :children (list unbounded-section)))
             (limited-section (make-node :section :begin 0 :end 10
                                         :children (list (make-node :paragraph
                                                                    :properties
                                                                    '(:value "body")))))
             (limited-heading (make-node :headline :children (list limited-section)))
             (limited-body
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-headline-body
                   limited-heading '((:include-body . t))
                   (make-limits :max-output-length 5))))))
        (setf (document-root document) root)
        (with-soft-assertions
          (expect (org-kit::%icalendar-node-text plain) :to-equal "plain")
          (expect (org-kit::%icalendar-node-text empty-plain) :to-equal "")
          (expect (org-kit::%icalendar-node-text entity-utf8) :to-equal "utf8")
          (expect (org-kit::%icalendar-node-text entity-unicode) :to-equal "unicode")
          (expect (org-kit::%icalendar-node-text attached-entity) :to-equal "E")
          (expect (org-kit::%icalendar-node-text line-break) :to-equal (string #\Newline))
          (expect (org-kit::%icalendar-node-text active) :to-equal "<date>")
          (expect (org-kit::%icalendar-node-text inactive) :to-equal "")
          (expect (org-kit::%icalendar-node-text inactive '((:with-timestamps . :inactive)))
                  :to-equal "[date]")
          (expect (org-kit::%icalendar-node-text inactive '((:with-timestamps . :all)))
                  :to-equal "[date]")
          (expect (org-kit::%icalendar-node-text path-link) :to-equal "target")
          (expect (org-kit::%icalendar-node-text text-link) :to-equal "plain")
          (expect (org-kit::%icalendar-node-text ignored) :to-equal "")
          (expect (org-kit::%icalendar-node-text generic-child) :to-equal "plain")
          (expect (org-kit::%icalendar-node-text generic-value) :to-equal "fallback")
          (expect (org-kit::%icalendar-headline-title title-heading)
                  :to-equal "prefixplain")
          (expect (org-kit::%icalendar-headline-title fallback-heading) :to-equal "raw")
          (expect (org-kit::%icalendar-headline-title empty-heading) :to-equal "")
          (expect body :to-equal "a  b  c  d")
          (expect short-body :to-equal "a  ")
          (expect (search (string #\Newline)
                          (org-kit::%icalendar-headline-body
                           wrapped-heading '((:include-body . t)) (make-limits)))
                  :to-be-truthy)
          (expect (org-kit::%icalendar-headline-body
                   empty-heading '((:include-body . t)) (make-limits))
                  :to-be nil)
          (expect (org-kit::%icalendar-headline-body
                   unbounded-heading '((:include-body . t)) (make-limits))
                  :to-equal "body")
          (expect (typep limited-body 'org-resource-limit-error) :to-be t))))

  (it "covers timestamp selection, category selectors, exclusion tags, and TODO modes"
      (let* ((active (make-node :timestamp :properties '(:type :active :value "<active>")))
             (title-active
               (make-node :timestamp :properties '(:type :active :value "<title>")))
             (inactive (make-node :timestamp :properties '(:type :inactive :value "[inactive]")))
             (child-time (make-node :timestamp :properties '(:type :active :value "<child>")))
             (child-heading (make-node :headline :children (list child-time)))
             (parent (make-node :headline
                                :properties '(:level 1)
                                :children (list active inactive
                                                (make-node :planning :children
                                                           (list (make-node :timestamp
                                                                            :properties
                                                                            '(:type :active))))
                                                child-heading)))
             (todo (make-node :headline :properties
                             '(:todo-keyword "TODO" :todo-type :todo)))
             (done (make-node :headline :properties
                             '(:todo-keyword "DONE" :todo-type :done)))
             (plain (make-node :headline))
             (blocked (make-node :headline :properties
                                 '(:todo-keyword "TODO" :todo-type :todo)
                                 :children (list (make-node :headline :properties
                                                           '(:todo-keyword "TODO"
                                                             :todo-type :todo)))))
             (document
               (parse-org
                (format nil
                        "#+TODO: TODO | DONE~%#+CATEGORY: Projects~%#+PROPERTY: TAGS global-tag~%* TODO Tagged :local:~%:PROPERTIES:~%:ID: tagged~%:END:~%" )
                :source-name "calendar/tasks.org"))
             (document-heading nil)
             (categories nil)
             (unblocked-options '((:include-todo . :unblocked)))
             (repeater (lambda (unit)
                         (make-node :timestamp
                                    :properties
                                    (list :repeater-type :cumulate
                                          :repeater-value 2
                                          :repeater-unit unit))))
             (repeater-values
               (mapcar (lambda (unit)
                         (org-kit::%icalendar-repeater-rule (funcall repeater unit)))
                       '(:hour :day :week :month :year :unknown))))
        (org-kit::map-nodes
         (lambda (node)
           (when (and (eq (node-type node) :headline)
                      (node-property node :todo-keyword))
             (setf document-heading node)))
         (document-root document))
        (when document-heading
          (setf categories
                (org-kit::%icalendar-categories
                 document document-heading
                 '((:categories . ("local-tags" "category" "todo-state" "all-tags"))))))
        (with-soft-assertions
          (expect (length (org-kit::%icalendar-timestamp-list
                           parent '((:with-timestamps . :active))))
                  :to-be 1)
          (expect (length (org-kit::%icalendar-timestamp-list
                           parent '((:with-timestamps . :inactive))))
                  :to-be 1)
          (expect (length (org-kit::%icalendar-timestamp-list
                           parent '((:with-timestamps . :all))))
                  :to-be 2)
          (expect (length (org-kit::%icalendar-timestamp-list
                           parent '((:with-timestamps . :active-exclude-diary))))
                  :to-be 1)
          (expect (length
                   (org-kit::%icalendar-timestamp-list
                    (make-node :headline :properties (list :title (list title-active)))
                    '((:with-timestamps . :active))))
                  :to-be 1)
          (expect (org-kit::%icalendar-excluded-heading-p
                   (make-node :headline :properties '(:tags ("work" :personal)))
                   '((:exclude-tags . (:work))))
                  :to-be t)
          (expect (org-kit::%icalendar-excluded-heading-p
                   (make-node :headline :properties '(:tags ("work")))
                   '((:exclude-tags . ("other"))))
                  :to-be nil)
          (expect (member "local" categories :test #'string-equal) :to-be-truthy)
          (expect (member "Projects" categories :test #'string-equal) :to-be-truthy)
          (expect (member "TODO" categories :test #'string-equal) :to-be-truthy)
          (expect (member "global-tag" categories :test #'string-equal) :to-be-truthy)
          (expect (org-kit::%icalendar-category-name document) :to-equal "Projects")
          (expect (org-kit::%icalendar-category-name (parse-org "")) :to-equal "OrgMode")
          (expect (org-kit::%icalendar-source-base-name
                   (parse-org "" :source-name "C:\\folder\\agenda.org"))
                  :to-equal "agenda")
          (expect (org-kit::%icalendar-todo-included-p
                   todo '((:include-todo . t))) :to-be t)
          (expect (org-kit::%icalendar-todo-included-p
                   todo '((:include-todo . nil))) :to-be nil)
          (expect (org-kit::%icalendar-todo-included-p
                   todo '((:include-todo . ("TODO")))) :to-be-truthy)
          (expect (org-kit::%icalendar-todo-included-p
                   done '((:include-todo . t))) :to-be nil)
          (expect (org-kit::%icalendar-todo-included-p
                   blocked unblocked-options) :to-be nil)
          (expect (org-kit::%icalendar-todo-included-p
                   plain '((:include-todo . :all))) :to-be nil)
          (expect (org-kit::%icalendar-entry-event-enabled-p nil '(:event-if-not-todo))
                  :to-be-truthy)
          (expect (org-kit::%icalendar-entry-event-enabled-p :todo '(:event-if-todo))
                  :to-be-truthy)
          (expect (org-kit::%icalendar-entry-event-enabled-p :todo
                                                             '(:event-if-todo-not-done))
                  :to-be-truthy)
          (expect (org-kit::%icalendar-entry-event-enabled-p :done '(:event-if-todo))
                  :to-be-truthy)
          (expect (org-kit::%icalendar-entry-event-enabled-p :done nil) :to-be nil)
          (expect repeater-values
                  :to-equal '("FREQ=HOURLY;INTERVAL=2" "FREQ=DAILY;INTERVAL=2"
                              "FREQ=WEEKLY;INTERVAL=2" "FREQ=MONTHLY;INTERVAL=2"
                              "FREQ=YEARLY;INTERVAL=2" nil))
          (expect (org-kit::%icalendar-repeater-rule (funcall repeater :day) t) :to-equal "FREQ=DAILY;INTERVAL=2")))))

(describe "iCalendar rendering and output coverage"
  (it "covers timestamp formatting branches, date bounds, event time, and prefixes"
      (let* ((time-context
               (export-context-time-context (export-icalendar-test-context)))
             (date-time
               (make-node :timestamp
                          :properties
                          '(:type :active :year-start 2024 :month-start 3
                            :day-start 14 :hour-start 9 :minute-start 5)))
             (date-only
               (make-node :timestamp
                          :properties
                          '(:type :active :year-start 2024 :month-start 3
                            :day-start 14)))
             (range
               (make-node :timestamp
                          :properties
                          '(:type :active-range :year-start 2024 :month-start 3
                            :day-start 14 :hour-start 9 :minute-start 5
                            :year-end 2024 :month-end 3 :day-end 14
                            :hour-end 10 :minute-end 15)))
             (builder (org-kit::%make-icalendar-builder :limits (make-limits)))
             (format-options
               '((:date-time-format . ";X=%Z:%Y-%y-%m-%dT%H:%M:%S%%")))
             (bad-format
               (lambda (format-string timestamp)
                 (export-icalendar-test-catch-error
                  (lambda ()
                    (org-kit::%icalendar-timestamp-property
                     (org-kit::%make-icalendar-builder :limits (make-limits))
                     "DTSTART" timestamp nil
                     (list (cons :date-time-format format-string))
                     time-context)))))
             (no-separator (funcall bad-format "plain" date-time))
             (trailing-percent (funcall bad-format ":%" date-time))
             (unknown-directive (funcall bad-format ":%Q" date-time))
             (date-directive-in-prefix (funcall bad-format "%Y:%H" date-time))
             (control-in-format
               (funcall bad-format (format nil ":%Y~%") date-time))
             (date-time-utc-builder
               (org-kit::%make-icalendar-builder :limits (make-limits)))
             (no-context-error
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-timestamp-property
                   (org-kit::%make-icalendar-builder :limits (make-limits))
                   "DTSTART" date-time "UTC" nil nil))))
             (invalid-timestamp
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-timestamp-property
                   (org-kit::%make-icalendar-builder :limits (make-limits))
                   "DTSTART" (make-node :timestamp) nil nil time-context)))))
        (org-kit::%icalendar-timestamp-property
         builder "DTSTART" date-time nil format-options time-context)
        (org-kit::%icalendar-timestamp-property
         builder "DTEND" date-time "Europe/Paris" nil time-context
         :exclusive-end-p t)
        (org-kit::%icalendar-timestamp-property
         builder "DUE" date-only nil nil time-context)
        (org-kit::%icalendar-timestamp-property
         builder "DTEND" date-only nil nil time-context :exclusive-end-p t)
        (org-kit::%icalendar-timestamp-property
         date-time-utc-builder "DTSTART" date-time nil
         '((:date-time-format . ":%Y%m%dT%H%M%SZ")) time-context)
        (with-soft-assertions
          (expect (search "DTSTART;X=UTC:2024-24-03-14T09:05:00%"
                          (coerce (org-kit::%icalendar-builder-buffer builder)
                                  'string))
                  :to-be-truthy)
          (expect (search "DTEND;TZID=Europe/Paris:20240314T110500"
                          (coerce (org-kit::%icalendar-builder-buffer builder)
                                  'string))
                  :to-be-truthy)
          (expect (search "DUE;VALUE=DATE:20240314"
                          (coerce (org-kit::%icalendar-builder-buffer builder)
                                  'string))
                  :to-be-truthy)
          (expect (search "DTEND;VALUE=DATE:20240315"
                          (coerce (org-kit::%icalendar-builder-buffer builder)
                                  'string))
                  :to-be-truthy)
          (expect (search "DTSTART:20240314T090500Z"
                          (coerce
                           (org-kit::%icalendar-builder-buffer date-time-utc-builder)
                           'string))
                  :to-be-truthy)
          (expect (cl-date-kit:local-date-time-hour
                   (org-kit::%icalendar-event-end date-time
                                                  (timestamp-start date-time)))
                  :to-be 11)
          (expect (cl-date-kit:local-date-time-hour
                   (org-kit::%icalendar-event-end range
                                                  (timestamp-start range)))
                  :to-be 10)
          (expect (org-kit::%icalendar-date-value
                   (org-kit::%time-value-from-local-value (timestamp-start date-only)) t)
                  :to-equal "20240315")
          (expect (org-kit::%icalendar-summary-prefix
                   '(:kind :event :scheduled t)
                   '((:summary-prefix-scheduled . "S: ")))
                  :to-equal "S: ")
          (expect (org-kit::%icalendar-summary-prefix
                   '(:kind :event :deadline t)
                   '((:summary-prefix-deadline . "DL: ")))
                  :to-equal "DL: ")
          (expect (org-kit::%icalendar-summary-prefix '(:kind :todo :scheduled t) nil)
                  :to-equal "")
          (expect (typep no-separator 'org-export-error) :to-be t)
          (expect (typep trailing-percent 'org-export-error) :to-be t)
          (expect (typep unknown-directive 'org-export-error) :to-be t)
          (expect (typep date-directive-in-prefix 'org-export-error) :to-be t)
          (expect (typep control-in-format 'org-export-error) :to-be t)
          (expect (typep no-context-error 'org-time-error) :to-be t)
          (expect (typep invalid-timestamp 'org-time-error) :to-be t))))

  (it "covers planning start rules, event rendering, TODO status, and alarms"
      (let* ((context (export-icalendar-test-context))
             (time-context (export-context-time-context context))
             (document
               (parse-org
                (format nil
                        "#+TODO: TODO | DONE~%* TODO Meeting :local:~%SCHEDULED: <2024-03-14 Thu 09:00 +1w>~%:PROPERTIES:~%:ID: meeting~%:LOCATION: Room 1~%:CLASS: PRIVATE~%:APPT_WARNTIME: 15~%:END:~%Body text~%* DONE Complete~%:PROPERTIES:~%:ID: complete~%:END:~%")))
             (meeting nil)
             (complete nil)
             (test-options
               '((:include-todo . :all)
                 (:use-scheduled . (:todo-start :event-if-todo))
                 (:date-time-format . ":%Y%m%dT%H%M%S")
                 (:summary-prefix-scheduled . "S: ")))
             (deadline
               (make-node :timestamp
                          :properties
                          '(:type :active :year-start 2024 :month-start 4
                            :day-start 12 :warning-value 2 :warning-unit :day
                            :warning-type :all)))
             (warning-heading
               (make-node :headline
                          :properties
                          (list :todo-keyword "TODO" :todo-type :todo
                                :deadline deadline)))
             (warning-options
               '((:include-todo . t) (:use-scheduled . (:todo-start))
                 (:use-deadline . (:todo-due))
                 (:todo-unscheduled-start . :deadline-warning)))
             (warning-events
               (org-kit::%icalendar-planning-events
                warning-heading warning-options time-context))
             (warning-todo (find-if (lambda (event) (eq (getf event :kind) :todo))
                                    warning-events))
             (warning-start (getf warning-todo :scheduled))
             (unit-days
               (mapcar
                (lambda (unit-and-value)
                  (let* ((timestamp
                           (make-node :timestamp
                                      :properties
                                      (list :type :active :year-start 2024
                                            :month-start 4 :day-start 12
                                            :warning-value (second unit-and-value)
                                            :warning-unit (first unit-and-value)
                                            :warning-type :all)))
                         (heading
                           (make-node :headline
                                      :properties
                                      (list :todo-keyword "TODO" :todo-type :todo
                                            :deadline timestamp)))
                         (events
                           (org-kit::%icalendar-planning-events
                            heading warning-options time-context))
                         (todo (find-if
                                (lambda (event) (eq (getf event :kind) :todo))
                                events)))
                    (org-kit::%icalendar-date-value
                     (org-kit::%time-value-from-local-value (timestamp-start (getf todo :scheduled))))))
                '((:hour 24) (:day 2) (:week 2) (:month 1) (:year 1)))) (default-warning-heading
               (make-node :headline
                          :properties
                          (list :todo-keyword "TODO" :todo-type :todo
                                :deadline
                                (make-node :timestamp
                                           :properties
                                           '(:type :active :year-start 2024
                                             :month-start 4 :day-start 12))))) (default-warning-events
               (org-kit::%icalendar-planning-events
                default-warning-heading warning-options time-context))) (org-kit::map-nodes
         (lambda (node)
           (when (eq (node-type node) :headline)
             (if (string= (node-property node :raw-value) "Meeting")
                 (setf meeting node)
                 (when (string= (node-property node :raw-value) "Complete")
                   (setf complete node)))))
         (document-root document)) (let* ((event (first
                       (org-kit::%icalendar-planning-events
                        meeting test-options time-context)))
               (todo-event
                 (find-if (lambda (candidate) (eq (getf candidate :kind) :todo))
                          (org-kit::%icalendar-planning-events
                           complete test-options time-context)))
               (render-builder
                 (org-kit::%make-icalendar-builder :limits (make-limits)))
               (todo-builder
                 (org-kit::%make-icalendar-builder :limits (make-limits)))
               (meeting-time (node-property meeting :scheduled))
               (public-output
                 (export-icalendar-documents
                  (list document)
                  :context context
                  :options '(:include-todo :all
                             :use-scheduled (:todo-start :event-if-todo)))))
          (org-kit::%icalendar-render-event render-builder document meeting event
                                           test-options time-context "meeting" nil)
          (org-kit::%icalendar-render-event todo-builder document complete todo-event
                                           test-options time-context "complete" nil)
          (let* ((rendered (coerce (org-kit::%icalendar-builder-buffer render-builder)
                                   'string))
                 (rendered-todo
                   (coerce (org-kit::%icalendar-builder-buffer todo-builder) 'string))
                 (alarm-builder
                   (org-kit::%make-icalendar-builder :limits (make-limits)))
                 (zero-alarm-builder
                   (org-kit::%make-icalendar-builder :limits (make-limits)))
                 (no-alarm-builder
                   (org-kit::%make-icalendar-builder :limits (make-limits))))
            (org-kit::%icalendar-event-alarms
             alarm-builder complete (list :scheduled meeting-time) "Meeting" '((:alarm-time . 0) (:force-alarm . nil)))
            (org-kit::%icalendar-event-alarms
             zero-alarm-builder complete (list :scheduled meeting-time) "Meeting" '((:alarm-time . 0) (:force-alarm . t)))
            (org-kit::%icalendar-event-alarms
             no-alarm-builder complete (list :timestamp nil) "Meeting" '((:alarm-time . 30) (:force-alarm . t)))
            (with-soft-assertions
              (progn
  (expect (node-property meeting :todo-type) :to-be :todo)
  (expect (node-property meeting :scheduled) :to-be-truthy))
              (expect (node-property meeting :raw-value) :to-equal "Meeting")
              (expect (node-property complete :raw-value) :to-equal "Complete")
              (expect (search "UID:SC-meeting" rendered) :to-be-truthy)
              (expect (search "SUMMARY:S: Meeting" rendered)
                      :to-be-truthy)
              (expect (search "LOCATION:Room 1" rendered) :to-be-truthy)
              (expect (search "CLASS:PRIVATE" rendered) :to-be-truthy)
              (expect (search "RRULE:FREQ=WEEKLY;INTERVAL=1" rendered)
                      :to-be-truthy)
              (expect (search "TRIGGER:-P0DT0H15M0S" rendered)
                      :to-be-truthy)
              (expect (search "STATUS:COMPLETED" rendered-todo) :to-be-truthy)
              (expect (search "SEQUENCE:1" rendered-todo) :to-be-truthy)
              (expect (search "PRIORITY:5" rendered-todo) :to-be-truthy)
              (expect (search "UID:SC-meeting" public-output) :to-be-truthy)
              (expect (search "UID:TODO-meeting" public-output) :to-be-truthy)
              (expect (search "UID:TODO-complete" public-output) :to-be-truthy)
              (expect (search "BEGIN:VALARM" (coerce
                                                (org-kit::%icalendar-builder-buffer alarm-builder)
                                                'string))
                      :to-be nil)
              (expect (search "BEGIN:VALARM" (coerce
                                                (org-kit::%icalendar-builder-buffer zero-alarm-builder)
                                                'string))
                      :to-be-truthy)
              (expect (search "TRIGGER:-P0DT0H0M0S" (coerce
                                                       (org-kit::%icalendar-builder-buffer zero-alarm-builder)
                                                       'string))
                      :to-be-truthy)
              (expect (search "BEGIN:VALARM" (coerce
                                                (org-kit::%icalendar-builder-buffer no-alarm-builder)
                                                'string))
                      :to-be nil)
              (expect (org-kit::%icalendar-date-value
                       (org-kit::%time-value-from-local-value (timestamp-start warning-start)))
                      :to-equal "20240410")
              (expect unit-days
                      :to-equal '("20240411" "20240410" "20240329"
                                  "20240313" "20230413"))
              (expect (getf (first default-warning-events) :scheduled)
                      :to-be-truthy)
              (expect (org-kit::%icalendar-org-priority
                       (make-node :headline :properties '(:priority #\A)))
                      :to-equal "1")
              (expect (org-kit::%icalendar-org-priority
                       (make-node :headline :properties '(:priority #\B)))
                      :to-equal "5")
              (expect (org-kit::%icalendar-org-priority
                       (make-node :headline :properties '(:priority #\C)))
                      :to-equal "9")))))

  (it "covers document fallbacks, stream kinds, and context extensions"
      (let* ((context (export-icalendar-test-context))
             (document
               (parse-org
                (format nil
                        "#+TITLE: Calendar title~%#+AUTHOR: Calendar author~%* Meeting~%:PROPERTIES:~%:ID: stream-id~%:END:~%<2024-03-14 Thu 09:00>~%")
                :source-name "agenda/meetings.org"))
             (string-output (make-string-output-stream))
             (binary-output
               (make-instance 'export-core-test-binary-output-stream
                              :element-type '(unsigned-byte 8)))
             (unsupported-output
               (make-instance 'export-core-test-binary-output-stream
                              :element-type 'bit))
             (text (export-icalendar-documents (list document) :context context))
             (stream-result
               (export-icalendar-documents (list document) :context context
                                           :output-type :stream
                                           :stream string-output))
             (binary-result
               (export-icalendar-documents (list document) :context context
                                           :output-type :stream
                                           :stream binary-output))
             (byte-result
               (export-icalendar-documents (list document) :context context
                                           :output-type :octets))
             (empty-output
               (export-icalendar-documents nil
                                           :options '(:combined-name "Empty"))) (context-error
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-context :bad-context nil)))) (missing-stream-error
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-output-stream-kind :stream nil)))) (non-stream-error
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-output-stream-kind :string string-output)))) (unsupported-stream-error
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-output-stream-kind :stream unsupported-output)))) (unsupported-output-error
               (export-icalendar-test-catch-error
                (lambda () (export-icalendar-documents nil :output-type :unknown)))) (metadata-context
               (make-export-context
                :title "Context title"
                :author "Context author"
                :time-context (export-context-time-context context)
                :extensions (list (cons :icalendar-options
                                        '(:combined-name "Context calendar"
                                          :ttl "P1D"))))) (extended-output
               (export-icalendar-documents nil :context metadata-context))) (with-soft-assertions
          (expect stream-result :to-be string-output)
          (expect (get-output-stream-string string-output) :to-equal text)
          (expect binary-result :to-be binary-output)
          (expect (export-core-test-stream-octets binary-output) :to-equalp (cl-codec-kit:string-to-octets text :encoding :utf-8))
          (expect byte-result :to-equalp (cl-codec-kit:string-to-octets text :encoding :utf-8))
          (expect (search "X-WR-CALNAME:meetings" text) :to-be-truthy)
          (expect (search "X-WR-CALDESC:Calendar title" text) :to-be-truthy)
          (expect (search "PRODID:-//Calendar author//" text) :to-be-truthy)
          (expect (search "X-WR-CALNAME:Empty" empty-output) :to-be-truthy)
          (expect (search "X-WR-CALNAME:Context calendar" extended-output)
                  :to-be-truthy)
          (expect (search "X-PUBLISHED-TTL:P1D" extended-output) :to-be-truthy)
          (expect (org-kit::%icalendar-document-keyword-value document "MISSING")
                  :to-be nil)
          (expect (org-kit::%icalendar-source-base-name
                   (parse-org "" :source-name "agenda"))
                  :to-equal "agenda")
          (expect (org-kit::%icalendar-source-base-name (parse-org "")) :to-be nil)
          (expect (org-kit::%icalendar-calendar-name nil nil) :to-equal "OrgMode")
          (expect (typep context-error 'type-error) :to-be t)
          (expect (typep missing-stream-error 'org-export-error) :to-be t)
          (expect (typep non-stream-error 'org-export-error) :to-be t)
          (expect (typep unsupported-stream-error 'org-export-error) :to-be t)
          (expect (typep unsupported-output-error 'org-export-error) :to-be t))))
))

(describe "iCalendar reachable coverage edges"
  (it "handles nil options, negative option bounds, and disabled timestamp selection"
      (let* ((document
               (parse-org
                (format nil
                        "* Event~%:PROPERTIES:~%:ID: disabled-time~%:END:~%<2024-03-14 Thu 09:00>~%")))
             (context (export-icalendar-test-context))
             (output
               (export-icalendar-documents
                (list document)
                :context context
                :options '(:with-timestamps nil)))
             (negative-body
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value :include-body -1))))
             (negative-integer
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-parse-option-value :alarm-time -1)))))
        (with-soft-assertions
          (expect (org-kit::%icalendar-parse-option-value :include-todo nil)
                  :to-be nil)
          (expect (org-kit::%icalendar-parse-option-value :categories nil)
                  :to-be nil)
          (expect (org-kit::%icalendar-parse-option-value :with-timestamps nil)
                  :to-be nil)
          (expect (typep negative-body 'org-export-error) :to-be t)
          (when (typep negative-body 'org-export-error)
            (expect (org-export-error-reason negative-body) :to-be :invalid-option))
          (expect (typep negative-integer 'org-export-error) :to-be t)
          (when (typep negative-integer 'org-export-error)
            (expect (org-export-error-reason negative-integer) :to-be :invalid-option))
          (expect (search "BEGIN:VEVENT" output) :to-be nil))))

  (it "handles a date-time deadline warning and equal explicit event endpoints"
      (let* ((time-context
               (export-context-time-context (export-icalendar-test-context)))
             (deadline
               (make-node :timestamp
                          :properties
                          '(:type :active :year-start 2024 :month-start 4
                            :day-start 12 :hour-start 9 :minute-start 30
                            :warning-value 2 :warning-unit :day :warning-type :all)))
             (heading
               (make-node :headline
                          :properties
                          (list :todo-keyword "TODO" :todo-type :todo
                                :deadline deadline)))
             (options
               '((:include-todo . t)
                 (:use-deadline . (:todo-due))
                 (:todo-unscheduled-start . :deadline-warning)))
             (events (org-kit::%icalendar-planning-events heading options time-context))
             (todo (find-if (lambda (event) (eq (getf event :kind) :todo)) events))
             (warning-start (timestamp-start (getf todo :scheduled)))
             (equal-range
               (make-node :timestamp
                          :properties
                          '(:type :active-range :year-start 2024 :month-start 3
                            :day-start 14 :hour-start 9 :minute-start 5
                            :year-end 2024 :month-end 3 :day-end 14
                            :hour-end 9 :minute-end 5)))
             (fallback-end
               (org-kit::%icalendar-event-end
                equal-range (timestamp-start equal-range))))
        (with-soft-assertions
          (expect (cl-date-kit:local-date-time-p warning-start) :to-be t)
          (expect (cl-date-kit:local-date-time-day warning-start) :to-be 10)
          (expect (cl-date-kit:local-date-time-hour warning-start) :to-be 9)
          (expect (cl-date-kit:local-date-time-minute warning-start) :to-be 30)
          (expect (cl-date-kit:local-date-time-hour fallback-end) :to-be 11)
          (expect (cl-date-kit:local-date-time-minute fallback-end) :to-be 5))))

  (it "filters commented and archived headings and preserves date-only timezone values"
      (let* ((document
               (parse-org
                (format nil
                        "* Visible~%:PROPERTIES:~%:ID: visible~%:TIMEZONE: Europe/Paris~%:END:~%<2024-03-14 Thu>~%* COMMENT Hidden~%:PROPERTIES:~%:ID: hidden-comment~%:END:~%<2024-03-15 Fri>~%* Archived :ARCHIVE:~%:PROPERTIES:~%:ID: hidden-archive~%:END:~%<2024-03-16 Sat>~%")))
             (output
               (export-icalendar-documents
                (list document)
                :context (export-icalendar-test-context)))
             (nested-diary-document
               (parse-org
                (format nil
                        "* Parent~%** Child literal range~%:PROPERTIES:~%:ID: child-range~%:END:~%<%%(diary-block 1 2 2025 1 4 2025)>~%")))
             (random-values
               (multiple-value-list (export-icalendar-test-random-source)))
             (random-source (first random-values))
             (random-calls (second random-values))
             (nested-diary-output
               (export-icalendar-documents
                (list nested-diary-document)
                :context (export-icalendar-test-context)
                :random-source random-source)))
        (with-soft-assertions
          (expect (search "UID:TS1-visible" output) :to-be-truthy)
          (expect (search "UID:TS1-hidden-comment" output) :to-be nil)
          (expect (search "UID:TS1-hidden-archive" output) :to-be nil)
          (expect (search "DTSTART;VALUE=DATE:20240314" output) :to-be-truthy)
          (expect (search "DTEND;VALUE=DATE:20240315" output) :to-be-truthy)
          (expect (search "TZID=Europe/Paris" output) :to-be nil)
          (expect (export-icalendar-test-count "BEGIN:VEVENT" nested-diary-output)
                  :to-be 1)
          (expect (search "UID:DS1-child-range" nested-diary-output)
                  :to-be-truthy)
          (expect (funcall random-calls) :to-be 0))))

  (it "covers character, octet, and invalid output stream paths"
      (let* ((character-stream (make-string-output-stream))
             (octet-stream
               (make-instance 'export-core-test-binary-output-stream
                              :element-type '(unsigned-byte 8)))
             (unsupported-stream
               (make-instance 'export-core-test-binary-output-stream
                              :element-type 'bit))
             (character-result
               (export-icalendar-documents nil
                                           :output-type :stream
                                           :stream character-stream))
             (octet-result
               (export-icalendar-documents nil
                                           :output-type :stream
                                           :stream octet-stream))
             (character-text (get-output-stream-string character-stream))
             (context-error
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-context :bad-context nil))))
             (missing-stream
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents nil :output-type :stream))))
             (stream-for-string
               (make-string-output-stream))
             (non-stream-output
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents nil
                                              :output-type :string
                                              :stream stream-for-string))))
             (bad-element-type
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents nil
                                              :output-type :stream
                                              :stream unsupported-stream))))
             (bad-output-type
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents nil :output-type :unknown)))))
        (with-soft-assertions
          (expect character-result :to-be character-stream)
          (expect (search "BEGIN:VCALENDAR" character-text) :to-be-truthy)
          (expect octet-result :to-be octet-stream)
          (expect (export-core-test-stream-octets octet-stream) :to-equalp (cl-codec-kit:string-to-octets character-text :encoding :utf-8))
          (expect (typep context-error 'type-error) :to-be t)
          (expect (typep missing-stream 'org-export-error) :to-be t)
          (expect (typep non-stream-output 'org-export-error) :to-be t)
          (expect (typep bad-element-type 'org-export-error) :to-be t)
          (expect (typep bad-output-type 'org-export-error) :to-be t)))))

(describe "iCalendar keyword calendar names and random source"
  (it "uses in-buffer calendar names and rejects invalid random octets"
      (let* ((document
               (parse-org
                (format nil
                        "#+ICALENDAR_COMBINED_NAME: Keyword calendar~%* Event~%:PROPERTIES:~%:ID: calendar-id~%:END:~%<2024-03-14 Thu 09:00>~%")))
             (output
               (export-icalendar-documents
                (list document)
                :context (export-icalendar-test-context)))
             (explicit-output
               (export-icalendar-documents
                (list document)
                :context (export-icalendar-test-context)
                :options '(:combined-name "Explicit calendar")))
             (unidentified
               (parse-org
                (format nil "* Event without an ID~%<2024-03-14 Thu 09:00>~%")))
             (invalid-random
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents
                   (list unidentified)
                   :context (export-icalendar-test-context)
                   :random-source (lambda () 256))))))
        (with-soft-assertions
          (expect (search "X-WR-CALNAME:Keyword calendar" output)
                  :to-be-truthy)
          (expect (search "X-WR-CALNAME:Explicit calendar" explicit-output)
                  :to-be-truthy)
          (expect (and (typep invalid-random 'org-export-error)
                       (eq (org-export-error-backend invalid-random) :icalendar)
                       (eq (org-export-error-reason invalid-random)
                           :invalid-random-source))
                  :to-be t)))))

(describe "iCalendar nil selector and clock requirements"
  (it "omits timestamp text for a nil selector and requires a TODO start context"
    (let* ((timestamp
             (make-node :timestamp
                        :properties '(:type :active :value "<2024-03-14 Thu>")))
           (timestamp-text
             (org-kit::%icalendar-node-text
              timestamp
              (list (cons :with-timestamps nil))))
           (document
             (parse-org
              (format nil "#+TODO: TODO | DONE~%* TODO Waiting~%")))
           (heading nil)
           (missing-context
             (progn
               (org-kit::map-nodes
                (lambda (node)
                  (when (and (eq (node-type node) :headline)
                             (node-property node :todo-keyword))
                    (setf heading node)))
                (document-root document))
               (export-icalendar-test-catch-error
                (lambda ()
                  (org-kit::%icalendar-planning-events
                   heading
                   (list (cons :include-todo t)
                         (cons :todo-unscheduled-start :current-datetime))
                   nil))))))
      (with-soft-assertions
        (expect timestamp-text :to-equal "")
        (expect (node-property heading :todo-type) :to-be :todo)
        (expect (typep missing-context 'org-time-error) :to-be t)
        (when (typep missing-context 'org-time-error)
          (expect (org-time-error-reason missing-context)
                  :to-be :invalid-time))))))

(describe "iCalendar deadline planning and event timezone"
  (it "renders non-TODO deadline events and rejects an invalid event timezone"
    (let* ((deadline-document
             (parse-org
              (format nil
                      "#+TODO: TODO | DONE~%* Deadline reminder~%SCHEDULED: <2024-03-14 Thu 09:00> DEADLINE: <2024-03-15 Fri 11:00>~%:PROPERTIES:~%:ID: deadline-event~%:END:~%")))
           (deadline-heading nil)
           (deadline-output
             (progn
               (org-kit::map-nodes
                (lambda (node)
                  (when (eq (node-type node) :headline)
                    (setf deadline-heading node)))
                (document-root deadline-document))
               (export-icalendar-documents
                (list deadline-document)
                :context (export-icalendar-test-context))))
           (bad-zone-document
             (parse-org
              (format nil
                      "* Invalid event zone~%:PROPERTIES:~%:ID: invalid-zone~%:TIMEZONE: UTC!~%:END:~%<2024-03-14 Thu 09:00>~%")))
           (bad-zone
             (export-icalendar-test-catch-error
              (lambda ()
                (export-icalendar-documents
                 (list bad-zone-document)
                 :context (export-icalendar-test-context))))))
      (with-soft-assertions
        (expect (node-type (node-property deadline-heading :deadline))
                :to-be :timestamp)
        (expect (export-icalendar-test-count "BEGIN:VEVENT" deadline-output)
                :to-be 1)
        (expect (search "UID:DL-deadline-event" deadline-output)
                :to-be-truthy)
        (expect (search "SUMMARY:DL: Deadline reminder" deadline-output)
                :to-be-truthy)
        (expect (and (typep bad-zone 'org-export-error)
                     (eq (org-export-error-backend bad-zone) :icalendar)
                     (eq (org-export-error-reason bad-zone) :invalid-timezone))
                :to-be t)))))

(describe "iCalendar malformed diary input"
  (it "reports unterminated diary expressions as unsupported syntax"
    (let* ((document
             (parse-org "%%(diary-block 1 2 2025 1 4 2025"))
           (diary-node-count 0)
           (unsupported
             (progn
               (org-kit::map-nodes
                (lambda (node)
                  (when (or (eq (node-type node) :diary-sexp)
                            (and (eq (node-type node) :timestamp)
                                 (eq (node-property node :type) :diary)))
                    (incf diary-node-count)))
                (document-root document))
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents
                   (list document)
                   :context (export-icalendar-test-context)))))))
      (with-soft-assertions
        (expect (plusp diary-node-count) :to-be t)
        (expect (typep unsupported 'org-export-unsupported-syntax)
                :to-be t)
        (when (typep unsupported 'org-export-unsupported-syntax)
          (expect (org-export-unsupported-syntax-backend unsupported)
                  :to-be :icalendar)
          (expect (org-export-unsupported-syntax-feature unsupported)
                  :to-be :diary-sexp)))))
  (it "rechecks diary syntax after the caller's random source mutates it"
    (let* ((document
             (parse-org
              (format nil
                      "* Event~%<2025-01-02 Thu 09:00>~%%%(diary-block 1 2 2025 1 4 2025)~%")))
           (diary-node nil)
           (random-calls 0)
           (unsupported
             (progn
               (org-kit::map-nodes
                (lambda (node)
                  (when (eq (node-type node) :diary-sexp)
                    (setf diary-node node)))
                (document-root document))
               (export-icalendar-test-catch-error
                (lambda ()
                  (export-icalendar-documents
                   (list document)
                   :context (export-icalendar-test-context)
                   :random-source
                   (lambda ()
                     (incf random-calls)
                     (when diary-node
                       (setf (node-property diary-node :value)
                             "%%(unknown 1)"))
                     0)))))))
      (with-soft-assertions
        (expect (node-p diary-node) :to-be t)
        (expect (plusp random-calls) :to-be t)
        (expect (and (typep unsupported 'org-export-unsupported-syntax)
                     (eq (org-export-unsupported-syntax-backend unsupported)
                         :icalendar)
                     (eq (org-export-unsupported-syntax-feature unsupported)
                         :diary-sexp))
                :to-be t)))))

(describe "iCalendar branch boundary cases"
  (it "validates documents and parses quoted and unrelated BIND options"
    (let* ((invalid-document
             (export-icalendar-test-catch-error
              (lambda ()
                (export-icalendar-documents
                 (list :not-a-document)
                 :context (export-icalendar-test-context)))))
           (document
             (parse-org
              (format nil "#+BIND: org-html-postamble nil~%#+BIND: org-icalendar t~%#+BIND: org-icalendar-description \"Quoted calendar\"~%")))
           (options (org-kit::%icalendar-keyword-options document)))
      (with-soft-assertions
        (expect (typep invalid-document 'org-export-error) :to-be t)
        (when (typep invalid-document 'org-export-error)
          (expect (org-export-error-reason invalid-document)
                  :to-be :invalid-documents))
        (expect options
                :to-equal '((:description . "Quoted calendar"))))))

  (it "escapes carriage returns in text and trims headline whitespace"
    (let* ((builder (org-kit::%make-icalendar-builder :limits (make-limits)))
           (heading
             (make-node
              :headline
              :children
              (list
               (make-node
                :section
                :children
                (list
                 (make-node :paragraph
                            :properties
                            (list :value (format nil "one two~Cthree~Cfour~C~C~C"
                                      #\Tab #\Return #\Space #\Return #\Tab))))))))
           (body
             (org-kit::%icalendar-headline-body
              heading '((:include-body . t)) (make-limits))))
      (org-kit::%icalendar-append-line
       builder
       (list (cons "TEXT:" nil)
             (cons (format nil "tail~C" #\Return) t)))
      (with-soft-assertions
        (expect (search "TEXT:tail\\n"
                        (coerce (org-kit::%icalendar-builder-buffer builder)
                                'string))
                :to-be-truthy)
        (expect body :to-equal (format nil "one two~Cthree  four" #\Tab)))))

  (it "omits inherited all-tags when no TAGS property exists"
    (let* ((document (parse-org (format nil "* Untagged~%")))
           (heading nil))
      (org-kit::map-nodes
       (lambda (node)
         (when (eq (node-type node) :headline)
           (setf heading node)))
       (document-root document))
      (with-soft-assertions
        (expect (org-kit::%icalendar-categories
                 document heading '((:categories . ("all-tags"))))
                :to-be nil))))

  (it "rejects invalid alarm, timezone, and diary inputs and checks zone names"
    (let* ((invalid-alarm
             (export-icalendar-test-catch-error
              (lambda ()
                (export-icalendar-documents
                 nil
                 :context (export-icalendar-test-context)
                 :options '(:alarm-time 1.5)))))
           (bad-zone-document
             (parse-org
              (format nil
                      "* Event~%:PROPERTIES:~%:ID: unicode-zone~%:TIMEZONE: 東京~%:END:~%<2024-03-14 Thu 09:00>~%")))
           (bad-zone
             (export-icalendar-test-catch-error
              (lambda ()
                (export-icalendar-documents
                 (list bad-zone-document)
                 :context (export-icalendar-test-context)))))
           (reversed-diary-document
             (parse-org
              (format nil
                      "* Reversed diary range~%:PROPERTIES:~%:ID: reversed-range~%:END:~%<%%(diary-block 1 4 2025 1 2 2025)>~%")))
           (reversed-diary
             (export-icalendar-test-catch-error
              (lambda ()
                (export-icalendar-documents
                 (list reversed-diary-document)
                 :context (export-icalendar-test-context))))))
      (with-soft-assertions
        (expect (and (typep invalid-alarm 'org-export-error)
                     (eq (org-export-error-reason invalid-alarm) :invalid-option))
                :to-be t)
        (expect (and (typep bad-zone 'org-export-error)
                     (eq (org-export-error-reason bad-zone) :invalid-timezone))
                :to-be t)
        (expect (and (typep reversed-diary 'org-export-unsupported-syntax)
                     (eq (org-export-unsupported-syntax-feature reversed-diary)
                         :diary-sexp))
                :to-be t)
        (expect (org-kit::%icalendar-safe-zone-name-p nil) :to-be t)
        (expect (org-kit::%icalendar-safe-zone-name-p 42) :to-be nil))))

  (it "accepts numeric priority codes in the priority helper"
    (with-soft-assertions
      (expect (org-kit::%icalendar-org-priority
               (make-node :headline :properties '(:priority 66)))
              :to-equal "5")))
  (it "covers absent and malformed BIND values and ordinary keywords"
    (let* ((empty-bind
             (multiple-value-list
              (org-kit::%icalendar-bind-option
               "org-icalendar-description ")))
           (short-quote
             (export-icalendar-test-catch-error
              (lambda ()
                (org-kit::%icalendar-bind-option
                 (concatenate 'string "org-icalendar-description "
                              (string #\"))))))
           (ordinary-options
             (org-kit::%icalendar-keyword-options
              (parse-org (format nil "#+TITLE: Plain~%")))))
      (with-soft-assertions
        (expect (org-kit::%icalendar-bind-option nil) :to-be nil)
        (expect empty-bind :to-equal '(:description ""))
        (expect (and (typep short-quote 'org-export-error)
                     (eq (org-export-error-reason short-quote) :invalid-option))
                :to-be t)
        (expect ordinary-options :to-be nil))))
  (it "skips non-string keyword keys and defaults export output to a string"
    (let* ((document (make-document ""))
           (keyword
             (make-node :keyword :properties '(:key 7 :value "unused")))
           (root (make-node :org-data :children (list keyword)))
           (output
             (export-icalendar-documents
              (list (parse-org (format nil "* Default output~%")))
              :context (export-icalendar-test-context))))
      (setf (document-root document) root)
      (with-soft-assertions
        (expect (org-kit::%icalendar-keyword-options document) :to-be nil)
        (expect (stringp output) :to-be t)
        (expect (search "BEGIN:VCALENDAR" output) :to-be-truthy)))))
