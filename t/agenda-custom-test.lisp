;;;; t/agenda-custom-test.lisp
(in-package #:cl-org-kit/test)

(defun agenda-custom-test-context ()
  (make-time-context :reference-time
                     (make-org-time-value "2025-01-01T12:00:00+00:00")
                     :zone
                     (resolve-time-zone "UTC")))

(defun agenda-custom-test-document (&optional limits)
  (parse-org
   (format nil
           "#+TODO: TODO NEXT | DONE~%* TODO Alpha :work:~%DEADLINE: <2025-01-01 Wed>~%* NEXT Beta :home:~%SCHEDULED: <2025-01-01 Wed>~%* DONE Closed~%")
   :limits
   (or limits (make-limits))))

(defun agenda-custom-test-error (function)
  (handler-case (progn
                  (funcall function)
                  nil)
    (error (condition)
      condition)))

(describe "Custom agenda views"
          (it "evaluates ordered agenda, TODO, match, and search definitions"
              (let* ((document (agenda-custom-test-document))
                     (time-context (agenda-custom-test-context))
                     (views
                      (agenda-custom (list document)
                                     (list
                                      (list :key
                                            "agenda"
                                            :description
                                            "Calendar"
                                            :type
                                            :agenda
                                            :start-date
                                            (make-org-time-value "2025-01-01")
                                            :end-date
                                            (make-org-time-value "2025-01-01")
                                            :options
                                            '(:sorting-strategy nil))
                                      (list :key :todo :type :todo)
                                      (list :key
                                            #\m
                                            :type
                                            :match
                                            :match
                                            "TODO!=\"DONE\""
                                            :options
                                            '(:sorting-strategy nil))
                                      (list :key "search" :type :search :search "+Alpha"))
                                     :time-context
                                     time-context)))
                (with-soft-assertions
                 (expect (mapcar #'agenda-custom-view-key views)
                         :to-equal
                         (list "agenda" :todo #\m "search"))
                 (expect (mapcar #'agenda-custom-view-type views)
                         :to-equal
                         '(:agenda :todo :match :search))
                 (expect (mapcar #'agenda-custom-view-description views)
                         :to-equal
                         (list "Calendar" nil nil nil))
                 (expect (mapcar #'agenda-custom-view-p views)
                         :to-equal
                         '(t t t t))
                 (expect
                  (mapcar
                   (lambda (view)
                     (if (agenda-result-p (agenda-custom-view-result view))
                         :agenda-result
                         :match-entries))
                   views)
                  :to-equal
                  '(:agenda-result :agenda-result :match-entries :match-entries))
                 (expect
                  (length
                   (agenda-result-entries
                    (agenda-custom-view-result (first views))))
                  :to-be
                  2)
                 (expect
                  (length
                   (agenda-result-entries
                    (agenda-custom-view-result (second views))))
                  :to-be
                  2)
                 (expect
                  (eq
                   (agenda-result-time-context
                    (agenda-custom-view-result (second views)))
                   time-context)
                  :to-be
                  t)
                 (expect (length (agenda-custom-view-result (third views)))
                         :to-be
                         2)
                 (expect
                  (mapcar
                   (lambda (entry)
                     (node-property (agenda-match-entry-node entry) :raw-value))
                   (agenda-custom-view-result (fourth views)))
                  :to-equal
                  '("Alpha")))))
          (it "passes ELisp forms only to the registered evaluator"
              (let* ((document (agenda-custom-test-document))
                     (callbacks (make-callback-registry))
                     (received nil))
                (register-callback callbacks
                                   :evaluator
                                   :emacs-lisp
                                   (lambda (expression)
                                     (setf received expression)
                                     "TODO!=\"DONE\""))
                (let ((views
                       (agenda-custom (list document)
                                      (list
                                       (list :key
                                             "match"
                                             :type
                                             :match
                                             :match
                                             (list :elisp "query-source")))
                                      :time-context
                                      (agenda-custom-test-context)
                                      :callbacks
                                      callbacks)))
                  (with-soft-assertions
                   (expect received :to-equal "query-source")
                   (expect (length (agenda-custom-view-result (first views)))
                           :to-be
                           2)))))
          (it
           "signals the named missing-evaluator condition and rejects non-string results"
           (let* ((document (agenda-custom-test-document))
                  (definition
                   (list
                    (list :key
                          "match"
                          :type
                          :match
                          :match
                          (list :elisp "query-source"))))
                  (missing
                   (agenda-custom-test-error
                    (lambda ()
                      (agenda-custom (list document)
                                     definition
                                     :time-context
                                     (agenda-custom-test-context)))))
                  (callbacks (make-callback-registry)))
             (register-callback callbacks
                                :evaluator
                                :emacs-lisp
                                (lambda (form)
                                  (declare (ignore form))
                                  :not-a-string))
             (let ((invalid-result
                    (agenda-custom-test-error
                     (lambda ()
                       (agenda-custom (list document)
                                      definition
                                      :time-context
                                      (agenda-custom-test-context)
                                      :callbacks
                                      callbacks)))))
               (with-soft-assertions
                (expect (typep missing 'org-elisp-evaluator-error) :to-be t)
                (expect (org-callback-error-reason missing)
                        :to-be
                        :unregistered)
                (expect (typep invalid-result 'org-agenda-error) :to-be t)
                (expect (org-agenda-error-reason invalid-result)
                        :to-be
                        :invalid-option)))))
          (it
           "rejects malformed definitions, duplicate keys, and unsupported options"
           (let* ((document (agenda-custom-test-document))
                  (context (agenda-custom-test-context))
                  (cases
                   (list (list :bad-plist (list :key "x" :type))
                         (list :bad-list
                               (cons (list :key "x" :type :todo) :tail))
                         (list :missing-key (list :type :todo))
                         (list :bad-key (list :key "" :type :todo))
                         (list :bad-type (list :key "x" :type :unknown))
                         (list :bad-description
                               (list :key "x" :type :todo :description 9))
                         (list :missing-range (list :key "x" :type :agenda))
                         (list :wrong-agenda-match
                               (list :key
                                     "x"
                                     :type
                                     :agenda
                                     :start-date
                                     (make-org-time-value "2025-01-01")
                                     :end-date
                                     (make-org-time-value "2025-01-01")
                                     :match
                                     "/!"))
                         (list :wrong-agenda-search
                               (list :key
                                     "x"
                                     :type
                                     :agenda
                                     :start-date
                                     (make-org-time-value "2025-01-01")
                                     :end-date
                                     (make-org-time-value "2025-01-01")
                                     :search
                                     "Alpha"))
                         (list :todo-start
                               (list :key
                                     "x"
                                     :type
                                     :todo
                                     :start-date
                                     (make-org-time-value "2025-01-01")))
                         (list :todo-end
                               (list :key
                                     "x"
                                     :type
                                     :todo
                                     :end-date
                                     (make-org-time-value "2025-01-01")))
                         (list :todo-search
                               (list :key "x" :type :todo :search "Alpha"))
                         (list :missing-match (list :key "x" :type :match))
                         (list :match-start
                               (list :key
                                     "x"
                                     :type
                                     :match
                                     :match
                                     "/!"
                                     :start-date
                                     (make-org-time-value "2025-01-01")))
                         (list :match-end
                               (list :key
                                     "x"
                                     :type
                                     :match
                                     :match
                                     "/!"
                                     :end-date
                                     (make-org-time-value "2025-01-01")))
                         (list :match-search
                               (list :key
                                     "x"
                                     :type
                                     :match
                                     :match
                                     "/!"
                                     :search
                                     "Alpha"))
                         (list :missing-search (list :key "x" :type :search))
                         (list :search-start
                               (list :key
                                     "x"
                                     :type
                                     :search
                                     :search
                                     "Alpha"
                                     :start-date
                                     (make-org-time-value "2025-01-01")))
                         (list :search-end
                               (list :key
                                     "x"
                                     :type
                                     :search
                                     :search
                                     "Alpha"
                                     :end-date
                                     (make-org-time-value "2025-01-01")))
                         (list :search-match
                               (list :key
                                     "x"
                                     :type
                                     :search
                                     :search
                                     "Alpha"
                                     :match
                                     "/!"))
                         (list :bad-options
                               (list :key
                                     "x"
                                     :type
                                     :todo
                                     :options
                                     '(:unknown t)))
                         (list :duplicate-property
                               (list :key
                                     "x"
                                     :type
                                     :todo
                                     :type
                                     :match
                                     :match
                                     "/!"))
                         (list :bad-key-type
                               (list :key 17 :type :todo))
                         (list :bad-elisp-form
                               (list :key "x"
                                     :type :match
                                     :match (list :elisp "query-source" "extra")))))
                  (failures
                   (mapcar
                    (lambda (case)
                      (agenda-custom-test-error
                       (lambda ()
                         (agenda-custom (list document)
                                        (list (second case))
                                        :time-context
                                        context))))
                    cases))
                  (duplicate-key
                   (agenda-custom-test-error
                    (lambda ()
                      (agenda-custom (list document)
                                     (list (list :key "same" :type :todo)
                                           (list :key
                                                 "same"
                                                 :type
                                                 :match
                                                 :match
                                                 "/!"))
                                     :time-context
                                     context))))
                  (bad-options-list
                   (agenda-custom-test-error
                    (lambda ()
                      (agenda-custom (list document)
                                     (list
                                      (list :key
                                            "x"
                                            :type
                                            :todo
                                            :options
                                            '(:sorting-strategy)))
                                     :time-context
                                     context))))
                  (bad-definition-list
                   (agenda-custom-test-error
                    (lambda ()
                      (agenda-custom (list document) :not-a-list)))))
             (with-soft-assertions
              (expect
               (every
                (lambda (condition)
                  (and (typep condition 'org-agenda-error)
                       (eq (org-agenda-error-reason condition) :invalid-option)))
                failures)
               :to-be
               t)
              (expect (typep duplicate-key 'org-agenda-error) :to-be t)
              (expect (org-agenda-error-reason duplicate-key)
                      :to-be
                      :invalid-option)
              (expect (typep bad-options-list 'org-agenda-error) :to-be t)
              (expect (typep bad-definition-list 'org-agenda-error) :to-be t)
              (expect (agenda-custom nil nil) :to-equal nil))))
          (it "limits the number of custom views and validates callbacks"
              (let* ((document
                      (agenda-custom-test-document
                       (make-limits :max-agenda-entries 1)))
                     (condition
                      (agenda-custom-test-error
                       (lambda ()
                         (agenda-custom (list document)
                                        (list (list :key "one" :type :todo)
                                              (list :key "two" :type :todo))
                                        :time-context
                                        (agenda-custom-test-context)))))
                     (invalid-callbacks
                      (agenda-custom-test-error
                       (lambda ()
                         (agenda-custom nil nil :callbacks :invalid)))))
                (with-soft-assertions
                 (expect (typep condition 'org-resource-limit-error) :to-be t)
                 (expect (org-resource-limit-error-limit-name condition)
                         :to-be
                         :max-agenda-entries)
                 (expect (typep invalid-callbacks 'type-error) :to-be t)))))
