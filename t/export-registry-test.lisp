;;;; t/export-registry-test.lisp
(in-package #:cl-org-kit/test)

(defun export-registry-test-registration-error (function)
  (handler-case
      (progn (funcall function) nil)
    (org-export-registration-error (condition) condition)))

(defun export-registry-test-signals-type-error-p (function)
  (handler-case
      (progn (funcall function) nil)
    (type-error () t)))

(defun export-registry-test-signals-program-error-p (function)
  (handler-case
      (progn (funcall function) nil)
    (program-error () t)))
(progn
  (defun export-registry-test-capture-output-check (function)
  (let ((condition nil)
        (events nil))
    (setf condition
          (handler-case
              (handler-bind
                  ((org-resource-limit-error
                     (lambda (limit-error)
                       (push (list :phase org-kit::*export-current-export-phase*
                                   :kind org-kit::*export-current-output-check-kind*
                                   :index org-kit::*export-current-output-check-index*
                                   :actual
                                   (org-resource-limit-error-actual limit-error))
                             events))))
                (progn (funcall function) nil))
            (org-resource-limit-error (limit-error) limit-error)))
    (values condition (nreverse events))))
  (defclass export-registry-test-failing-output-stream
      (sb-gray:fundamental-character-output-stream)
    ())
  (defmethod stream-element-type
      ((stream export-registry-test-failing-output-stream))
    (declare (ignore stream))
    (quote character))
  (defmethod sb-gray:stream-write-char
      ((stream export-registry-test-failing-output-stream) character)
    (declare (ignore character))
    (error "~A" "forced output write failure"))
  (defmethod sb-gray:stream-write-string
      ((stream export-registry-test-failing-output-stream) string
       &optional start end)
    (declare (ignore string start end))
    (error "~A" "forced output write failure")))

(defun export-registry-test-render-views (context)
  (with-output-to-string (output)
    (dolist (view (export-render-context-root-views context))
      (write-string (export-render-view context view) output))))

(defun export-registry-test-with-root-backend (name arguments function)
  (let ((registered-p nil))
    (unwind-protect
         (progn
           (apply #'register-export-backend name arguments)
           (setf registered-p t)
           (funcall function))
      (when registered-p
        (unregister-export-backend name)))))

(defun export-registry-test-with-backend-pair
    (parent child parent-arguments child-arguments function)
  (let ((parent-registered-p nil)
        (child-registered-p nil))
    (unwind-protect
         (progn
           (apply #'register-export-backend parent parent-arguments)
           (setf parent-registered-p t)
           (apply #'register-derived-export-backend child parent child-arguments)
           (setf child-registered-p t)
           (funcall function))
      (when child-registered-p
        (unregister-export-backend child))
      (when parent-registered-p
        (unregister-export-backend parent)))))

(defun export-registry-test-link-handler (path description frame context)
  (declare (ignore path frame context))
  (make-export-link-result :kind :link
                           :destination "https://example.test/"
                           :description description))

(describe "export backend registry"
  (it "classifies registration errors and validates callback types"
    (let ((backend :api-registry-registration-matrix)
          (registered-p nil))
      (unwind-protect
           (progn
             (with-soft-assertions
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-backend :bad_name
                                             :renderer #'identity))))
                :to-be :invalid-backend-name)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-backend :api-registry-no-renderer))))
                :to-be :invalid-renderer)
               (expect
                (export-registry-test-signals-type-error-p
                 (lambda ()
                   (register-export-backend :api-registry-bad-callback
                                            :renderer 42)))
                :to-be t)
               (expect
                (export-registry-test-signals-type-error-p
                 (lambda ()
                   (register-export-backend :api-registry-bad-map
                                            :renderer #'identity
                                            :transcoders :not-a-list)))
                :to-be t)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-backend
                     :api-registry-invalid-renderer-priority
                     :transcoders
                     (list (cons :unknown-node #'identity))))))
                :to-be :invalid-renderer)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-backend
                     :api-registry-invalid-transcoder
                     :renderer #'identity
                     :transcoders
                     (list (cons :unknown-node #'identity))))))
                :to-be :invalid-transcoder)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-backend
                     :api-registry-invalid-policy
                     :renderer #'identity
                     :node-policies
                     (list (cons :paragraph :unknown-policy))))))
                :to-be :invalid-node-policy)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-backend
                     :api-registry-invalid-filter-stage
                     :renderer #'identity
                     :filters
                     (list
                      (cons :unknown-stage (list #'identity)))))))
                :to-be :invalid-filter-stage)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-derived-export-backend
                     :api-registry-unknown-parent
                     :api-registry-missing-parent))))
                :to-be :unknown-parent)
               (register-export-backend backend :renderer #'identity)
               (setf registered-p t)
               (let ((duplicate
                       (export-registry-test-registration-error
                        (lambda ()
                          (register-export-backend
                           backend :renderer #'identity)))))
                 (expect (typep duplicate 'org-export-registration-error)
                         :to-be t)
                 (when duplicate
                   (expect (org-export-error-backend duplicate)
                           :to-be backend)
                   (expect (org-export-error-reason duplicate)
                           :to-be :duplicate-backend)))
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-link-type
                     :api-registry-missing-backend "sample" #'identity))))
                :to-be :unknown-backend)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda ()
                    (register-export-link-type backend "" #'identity))))
                :to-be :invalid-link-type-handler)
               (expect
                (export-registry-test-signals-type-error-p
                 (lambda ()
                   (register-export-link-type backend "sample" 42)))
                :to-be t)))
        (when registered-p
          (unregister-export-backend backend)))))

  (it "inherits renderers and node maps while checking strict ancestry"
    (let* ((root :api-registry-inheritance-root)
           (child :api-registry-inheritance-child)
           (grandchild :api-registry-inheritance-grandchild)
           (root-registered-p nil)
           (child-registered-p nil)
           (grandchild-registered-p nil)
           (renderer-owner nil)
           (document (parse-org "Registry inheritance.")))
      (unwind-protect
           (progn
             (register-export-backend
              root
              :renderer (lambda (context)
                          (setf renderer-owner
                                (export-render-context-owner-backend context))
                          (export-registry-test-render-views context))
              :transcoders
              (list (cons :paragraph
                          (lambda (node view frame context)
                            (declare (ignore node view frame context))
                            "parent"))))
             (setf root-registered-p t)
             (register-derived-export-backend
              child root
              :transcoders
              (list (cons :paragraph
                          (lambda (node view frame context)
                            (declare (ignore node view frame context))
                            "child"))))
             (setf child-registered-p t)
             (register-derived-export-backend grandchild child)
             (setf grandchild-registered-p t)
             (with-soft-assertions
               (expect (export-org document root) :to-equal "parent")
               (expect (export-org document child) :to-equal "child")
               (expect (export-org document grandchild) :to-equal "child")
               (expect renderer-owner :to-be root)
               (expect (export-backend-derived-p child root) :to-be t)
               (expect (export-backend-derived-p grandchild root) :to-be t)
               (expect (export-backend-derived-p grandchild child) :to-be t)
               (expect (export-backend-derived-p root root) :to-be nil)
               (expect (export-backend-derived-p child child) :to-be nil)
               (expect (export-registry-test-signals-type-error-p
                        (lambda () (export-backend-derived-p 42 root)))
                       :to-be t)
               (expect (export-registry-test-signals-type-error-p
                        (lambda () (export-backend-derived-p child 42)))
                       :to-be t)))
        (when grandchild-registered-p
          (unregister-export-backend grandchild))
        (when child-registered-p
          (unregister-export-backend child))
        (when root-registered-p
          (unregister-export-backend root)))))

  (it "rejects node rendering while state factories run"
    (let ((render-rejected-p nil))
      (export-registry-test-with-root-backend
       :api-registry-state-render-guard
       (list :renderer (lambda (context)
                         (declare (ignore context))
                         "ready")
             :make-state
             (lambda (context)
               (setf render-rejected-p
                     (export-registry-test-signals-program-error-p
                      (lambda ()
                        (export-render-view
                         context
                         (first (export-render-context-root-views context))))))
               nil))
       (lambda ()
         (with-soft-assertions
           (expect (export-org (parse-org "Factory isolation.")
                               :api-registry-state-render-guard)
                   :to-equal "ready")
           (expect render-rejected-p :to-be t))))))

  (it "initializes owner state once in ancestry order"
    (let ((root :api-registry-state-root)
          (child :api-registry-state-child)
          (events nil))
      (export-registry-test-with-backend-pair
       root child
       (list :renderer
             (lambda (context)
               (push (list :renderer :root
                           (export-render-context-backend context)
                           (export-render-context-owner-backend context)
                           (export-render-context-state context))
                     events)
               (if (eq (export-render-context-state context) :root-state)
                   "root-state"
                   "wrong-root-state"))
             :make-state
             (lambda (context)
               (push (list :factory :root
                           (export-render-context-backend context)
                           (export-render-context-owner-backend context)
                           (export-render-context-state context))
                     events)
               :root-state))
       (list :renderer
             (lambda (context)
               (push (list :renderer :child
                           (export-render-context-backend context)
                           (export-render-context-owner-backend context)
                           (export-render-context-state context))
                     events)
               (concatenate 'string
                            (if (eq (export-render-context-state context)
                                    :child-state)
                                "child-state"
                                "wrong-child-state")
                            "/"
                            (export-render-parent-renderer context)))
             :make-state
             (lambda (context)
               (push (list :factory :child
                           (export-render-context-backend context)
                           (export-render-context-owner-backend context)
                           (export-render-context-state context))
                     events)
               :child-state))
       (lambda ()
         (let ((output (export-org (parse-org "State ownership.") child)))
           (with-soft-assertions
             (expect output :to-equal "child-state/root-state")
             (expect
              (reverse events)
              :to-equal
              `((:factory :root ,child ,root nil)
                (:factory :child ,child ,child nil)
                (:renderer :child ,child ,child :child-state)
                (:renderer :root ,child ,root :root-state)))))))))

  (it "rejects backend removal in use and removes descendants before parents"
    (let ((root :api-registry-unregister-root)
          (child :api-registry-unregister-child)
          (root-registered-p nil)
          (child-registered-p nil))
      (unwind-protect
           (progn
             (with-soft-assertions
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda () (unregister-export-backend :api-registry-absent))))
                :to-be :unknown-backend)
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda () (unregister-export-backend :html))))
                :to-be :builtin-backend))
             (register-export-backend root :renderer #'identity)
             (setf root-registered-p t)
             (register-derived-export-backend child root)
             (setf child-registered-p t)
             (with-soft-assertions
               (expect
                (org-export-error-reason
                 (export-registry-test-registration-error
                  (lambda () (unregister-export-backend root))))
                :to-be :backend-in-use)
               (expect (unregister-export-backend child) :to-be child)
               (setf child-registered-p nil)
               (expect (unregister-export-backend root) :to-be root)
               (setf root-registered-p nil)
               (expect (export-backend-derived-p child root) :to-be nil)))
        (when child-registered-p
          (unregister-export-backend child))
        (when root-registered-p
          (unregister-export-backend root)))))

  (it "reports the requested backend for inherited preparation errors"
    (let ((backend :api-registry-error-identity)
          (registered-p nil)
          (condition nil))
      (unwind-protect
           (progn
             (register-derived-export-backend backend :gfm)
             (setf registered-p t
                   condition
                   (handler-case
                       (progn
                         (export-org (parse-org "#+UNKNOWN: value") backend)
                         nil)
                     (org-export-unsupported-syntax (error) error)))
             (with-soft-assertions
               (expect (typep condition 'org-export-unsupported-syntax)
                       :to-be t)
               (when (typep condition 'org-export-unsupported-syntax)
                 (expect (org-export-unsupported-syntax-backend condition)
                         :to-be backend)
                 (expect (org-export-unsupported-syntax-feature condition)
                         :to-be :unknown-keyword))))
        (when registered-p
          (unregister-export-backend backend)))))

  (it "runs derived filters before parent filters and handles nil and empty results"
    (let ((events nil)
          (parent-after-empty-ran-p nil)
          (document (parse-org "Filter order.")))
      (export-registry-test-with-backend-pair
       :api-registry-filter-parent :api-registry-filter-child
       (list :renderer (lambda (context)
                         (declare (ignore context))
                         "body")
             :filters
             (list
              (cons :body
                    (list (lambda (value context)
                            (declare (ignore context))
                            (push :parent-first events)
                            (concatenate 'string value "/parent"))
                          (lambda (value context)
                            (declare (ignore value context))
                            (push :parent-empty events)
                            "")
                          (lambda (value context)
                            (declare (ignore context))
                            (setf parent-after-empty-ran-p t)
                            value)))))
       (list :filters
             (list
              (cons :body
                    (list (lambda (value context)
                            (declare (ignore value context))
                            (push :child-nil events)
                            nil)
                          (lambda (value context)
                            (declare (ignore context))
                            (push :child-second events)
                            (concatenate 'string value "/child"))))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org document :api-registry-filter-child) :to-equal "")
           (expect (reverse events)
                   :to-equal '(:child-nil :child-second
                               :parent-first :parent-empty))
           (expect parent-after-empty-ran-p :to-be nil))))))

  (it "applies options and parse-tree filters at their pipeline boundaries"
    (let ((stages nil)
          (document (parse-org "Filter stages.")))
      (export-registry-test-with-root-backend
       :api-registry-preparation-filters
       (list :renderer
             (lambda (context)
               (format nil "~a/~d/~a"
                       (export-option-value context :test-option)
                       (length (export-render-context-root-views context))
                       (if (export-option-explicit-p context :test-option)
                           "explicit"
                           "default")))
             :option-specifications
             (list (list :key :test-option :token "Q" :parser #'identity :default "default"))
             :filters
             (list
              (cons :options
                    (list (lambda (options context)
                            (declare (ignore context))
                            (push :options stages)
                            (acons :test-option "filtered" options))))
              (cons :parse-tree
                    (list (lambda (views context)
                            (declare (ignore context))
                            (push :parse-tree stages)
                            (append views views))))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org document :api-registry-preparation-filters)
                   :to-equal "filtered/2/explicit")
           (expect (reverse stages) :to-equal '(:options :parse-tree)))))))

  (it "initializes callback state from filtered prepared views"
    (let ((state-observation nil)
          (document (parse-org "State context.")))
      (export-registry-test-with-root-backend
       :api-registry-filtered-state
       (list :renderer
             (lambda (context)
               (format nil "~d/~a"
                       (length (export-render-context-root-views context))
                       (getf (export-render-context-state context) :option)))
             :make-state
             (lambda (context)
               (setf state-observation
                     (list (export-render-context-options context)
                           (length (export-render-context-root-views context))))
               (list :option
                     (cdr (assoc :test-option
                                 (export-render-context-options context)))))
             :option-specifications
             (list (list :key :test-option :token "Q" :parser #'identity))
             :filters
             (list
              (cons :options
                    (list (lambda (options context)
                            (declare (ignore context))
                            (acons :test-option "filtered" options))))
              (cons :parse-tree
                    (list (lambda (views context)
                            (declare (ignore context))
                            (append views views))))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org document :api-registry-filtered-state)
                   :to-equal "2/filtered")
           (expect state-observation
                   :to-equal '(((:test-option . "filtered")) 2)))))))

  (it "returns independent metadata and option snapshots"
    (let ((metadata-isolated-p nil)
          (options-isolated-p nil))
      (export-registry-test-with-root-backend
       :api-registry-snapshot-copy
       (list :renderer
             (lambda (context)
               (let* ((metadata (export-render-context-metadata context))
                      (options (export-render-context-options context))
                      (title (getf metadata :title))
                      (option (cdr (assoc :test-option options))))
                 (setf (char title 0) #\X
                       (char option 0) #\X
                       metadata-isolated-p
                       (equal (getf (export-render-context-metadata context)
                                    :title)
                              "Stable title")
                       options-isolated-p
                       (equal (cdr (assoc :test-option
                                          (export-render-context-options context)))
                              "Stable-option"))
                 "isolated"))
             :option-specifications
             (list (list :key :test-option :token "Q" :parser (lambda (value source backend) (declare (ignore source backend)) value))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org
                    (parse-org
                     (format nil "#+TITLE: Stable title~%#+OPTIONS: Q:Stable-option~%~%Body"))
                    :api-registry-snapshot-copy)
                   :to-equal "isolated")
           (expect metadata-isolated-p :to-be t)
           (expect options-isolated-p :to-be t))))))

  (it "validates renderer results before filters can replace them"
    (let ((filter-ran-p nil))
      (export-registry-test-with-root-backend
       :api-registry-renderer-result-type
       (list :renderer
             (lambda (context)
               (declare (ignore context))
               nil)
             :filters
             (list (cons :body
                         (list (lambda (value context)
                                 (declare (ignore value context))
                                 (setf filter-ran-p t)
                                 "replaced")))))
       (lambda ()
         (with-soft-assertions
           (expect (export-registry-test-signals-type-error-p
                    (lambda ()
                      (export-org (parse-org "Renderer result.")
                                  :api-registry-renderer-result-type)))
                   :to-be t)
           (expect filter-ran-p :to-be nil))))))

  (it "uses the nearest complete option specification for defaults"
    (let ((root :api-registry-option-default-root)
          (child :api-registry-option-default-child))
      (export-registry-test-with-backend-pair
       root child
       (list :renderer
             (lambda (context)
               (or (export-option-value context :test-option) "no-default"))
             :option-specifications
             (list (list :key :test-option :token "Q" :parser #'identity
                         :default "parent-default")))
       (list :option-specifications
             (list (list :key :test-option :token "R" :parser #'identity)))
       (lambda ()
         (with-soft-assertions
           (expect (export-org (parse-org "Option default.") child)
                   :to-equal "no-default"))))))

  (it "parses child option tokens with the registered parser and requested backend"
    (let ((parent :api-registry-option-parser-parent)
          (child :api-registry-option-parser-child)
          (parsed-source nil)
          (parsed-backend nil))
      (export-registry-test-with-backend-pair
       parent child
       (list :renderer
             (lambda (context)
               (or (export-option-value context :test-option) "missing"))
             :option-specifications
             (list (list :key :test-option :token "base" :parser #'identity)))
       (list :option-specifications
             (list
              (list :key :test-option :token "custom"
                    :parser (lambda (value source backend)
                              (setf parsed-source source
                                    parsed-backend backend)
                              (concatenate 'string "child:" value)))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org
                    (parse-org (format nil "#+OPTIONS: custom:value~%~%Option parser.")) child)
                   :to-equal "child:value")
           (expect (node-type parsed-source) :to-be :keyword)
           (expect parsed-backend :to-be child))))))

  (it "copies supported mutable option defaults at registration and access"
    (let* ((backend :api-registry-option-default-copy)
           (default (vector "registered-value")))
      (export-registry-test-with-root-backend
       backend
       (list :renderer
             (lambda (context)
               (let ((first-value (export-option-value context :test-state)))
                 (setf (aref first-value 0) "callback-value")
                 (format nil "~A/~A"
                         (aref first-value 0)
                         (aref (export-option-value context :test-state) 0))))
             :option-specifications
             (list (list :key :test-state :token "state" :parser #'identity
                         :default default)))
       (lambda ()
         (setf (aref default 0) "caller-value")
         (with-soft-assertions
           (expect (export-org (parse-org "Option default copy.") backend)
                   :to-equal "callback-value/registered-value"))))))

  (it "rejects mutable option defaults without a copy rule"
    (let ((failure
            (export-registry-test-registration-error
             (lambda ()
               (register-export-backend
                :api-registry-option-default-unsupported
                :renderer #'identity
                :option-specifications
                (list (list :key :test-state :token "state" :parser #'identity
                            :default (make-hash-table))))))))
      (with-soft-assertions
        (expect (typep failure 'org-export-registration-error) :to-be t)
        (when failure
          (expect (org-export-error-reason failure)
                  :to-be :invalid-option-specification)))))

  (it "passes post-blank output to element filters"
    (let ((filtered-value nil))
      (export-registry-test-with-root-backend
       :api-registry-element-post-blank
       (list :renderer
             (lambda (context)
               (export-render-view
                context
                (make-export-view (make-node :paragraph :post-blank 2))))
             :transcoders
             (list (cons :paragraph
                         (lambda (node view frame context)
                           (declare (ignore node view frame context))
                           "item")))
             :filters
             (list (cons '(:element :paragraph)
                         (list (lambda (value context)
                                 (declare (ignore context))
                                 (setf filtered-value value)
                                 value)))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org (parse-org "Element filter.")
                               :api-registry-element-post-blank)
                   :to-equal (format nil "item~%~%"))
           (expect filtered-value :to-equal (format nil "item~%~%")))))))

  (it "enforces transcoder and wrapper return and continuation boundaries"
    (let ((wrapper-calls 0)
          (document (parse-org "Wrapper boundaries.")))
      (export-registry-test-with-root-backend
       :api-registry-valid-wrapper
       (list :renderer #'export-registry-test-render-views
             :transcoders
             (list (cons :paragraph
                         (lambda (node view frame context)
                           (declare (ignore node view frame context))
                           "node")))
             :node-wrapper
             (lambda (node view frame context render-next)
               (declare (ignore view frame context))
               (if (eq (node-type node) :paragraph)
                   (progn
                     (incf wrapper-calls)
                     (funcall render-next "<" ">"))
                   (funcall render-next "" ""))))
       (lambda ()
         (with-soft-assertions
           (expect (export-org document :api-registry-valid-wrapper)
                   :to-equal "<node>")
           (expect wrapper-calls :to-be 1))))
      (with-soft-assertions
        (expect
         (export-registry-test-with-root-backend
          :api-registry-bad-transcoder
          (list :renderer #'export-registry-test-render-views
                :transcoders
                (list (cons :paragraph
                            (lambda (node view frame context)
                              (declare (ignore node view frame context))
                              :not-a-string))))
          (lambda ()
            (export-registry-test-signals-type-error-p
             (lambda () (export-org document :api-registry-bad-transcoder)))))
         :to-be t)
        (expect
         (export-registry-test-with-root-backend
          :api-registry-zero-continuation
          (list :renderer #'export-registry-test-render-views
                :transcoders
                (list (cons :paragraph
                            (lambda (node view frame context)
                              (declare (ignore node view frame context))
                              "node")))
                :node-wrapper
                (lambda (node view frame context render-next)
                  (declare (ignore view frame context))
                  (if (eq (node-type node) :paragraph)
                      nil
                      (funcall render-next "" ""))))
          (lambda ()
            (export-registry-test-signals-program-error-p
             (lambda () (export-org document :api-registry-zero-continuation)))))
         :to-be t)
        (expect
         (export-registry-test-with-root-backend
          :api-registry-multiple-continuations
          (list :renderer #'export-registry-test-render-views
                :transcoders
                (list (cons :paragraph
                            (lambda (node view frame context)
                              (declare (ignore node view frame context))
                              "node")))
                :node-wrapper
                (lambda (node view frame context render-next)
                  (declare (ignore view frame context))
                  (if (eq (node-type node) :paragraph)
                      (progn
                        (funcall render-next "" "")
                        (funcall render-next "" ""))
                      (funcall render-next "" ""))))
          (lambda ()
            (export-registry-test-signals-program-error-p
             (lambda ()
               (export-org document :api-registry-multiple-continuations)))))
         :to-be t)
        (expect
         (export-registry-test-with-root-backend
          :api-registry-non-nil-wrapper
          (list :renderer #'export-registry-test-render-views
                :transcoders
                (list (cons :paragraph
                            (lambda (node view frame context)
                              (declare (ignore node view frame context))
                              "node")))
                :node-wrapper
                (lambda (node view frame context render-next)
                  (declare (ignore view frame context))
                  (funcall render-next "" "")
                  (if (eq (node-type node) :paragraph) :unexpected nil)))
          (lambda ()
            (export-registry-test-signals-type-error-p
             (lambda () (export-org document :api-registry-non-nil-wrapper)))))
         :to-be t))))

  (it "validates parent node and view helper inputs"
    (let ((node-mode-error nil)
          (node-boolean-error nil)
          (node-view-error nil)
          (view-mode-error nil)
          (view-boolean-error nil)
          (wrong-view (make-export-view (make-node :plain-text))))
      (export-registry-test-with-backend-pair
       :api-registry-parent-helper-base :api-registry-parent-helper-child
       (list :renderer #'export-registry-test-render-views
             :transcoders
             (list (cons :paragraph
                         (lambda (node view frame context)
                           (declare (ignore node view frame context))
                           "parent"))))
       (list :transcoders
             (list (cons :paragraph
                         (lambda (node view frame context)
                           (declare (ignore frame))
                           (setf node-mode-error
                                 (export-registry-test-signals-type-error-p
                                  (lambda ()
                                    (export-render-parent-node
                                     context node :mode :invalid))))
                           (setf node-boolean-error
                                 (export-registry-test-signals-type-error-p
                                  (lambda ()
                                    (export-render-parent-node
                                     context node :table-cell-p :yes))))
                           (setf node-view-error
                                 (export-registry-test-signals-type-error-p
                                  (lambda ()
                                    (export-render-parent-node
                                     context node :view wrong-view))))
                           (setf view-mode-error
                                 (export-registry-test-signals-type-error-p
                                  (lambda ()
                                    (export-render-parent-view
                                     context view :mode :invalid))))
                           (setf view-boolean-error
                                 (export-registry-test-signals-type-error-p
                                  (lambda ()
                                    (export-render-parent-view
                                     context view :preserve-source-breaks-p :yes))))
                           "child"))))
       (lambda ()
         (export-org (parse-org "Parent helper inputs.")
                     :api-registry-parent-helper-child)
         (with-soft-assertions
           (expect node-mode-error :to-be t)
           (expect node-boolean-error :to-be t)
           (expect node-view-error :to-be t)
           (expect view-mode-error :to-be t)
           (expect view-boolean-error :to-be t))))))

  (it "registers and removes backend-local link handlers"
    (let ((parent :api-registry-link-parent)
          (child :api-registry-link-child)
          (parent-handler-registered-p nil)
          (child-handler-registered-p nil))
      (export-registry-test-with-backend-pair
       parent child
       (list :renderer #'identity)
       nil
       (lambda ()
         (unwind-protect
              (progn
                (register-export-link-type
                 parent "custom" #'export-registry-test-link-handler)
                (setf parent-handler-registered-p t)
                (register-export-link-type
                 child "CUSTOM" #'export-registry-test-link-handler)
                (setf child-handler-registered-p t)
                (with-soft-assertions
                  (expect
                   (org-export-error-reason
                    (export-registry-test-registration-error
                     (lambda ()
                       (register-export-link-type
                        child "custom" #'export-registry-test-link-handler))))
                   :to-be :duplicate-link-type-handler)
                  (multiple-value-bind (handler owner)
                      (org-kit::export-resolved-link-handler
                       (org-kit::export-backend-ancestry child) "custom")
                    (expect (eq handler #'export-registry-test-link-handler)
                            :to-be t)
                    (expect owner :to-be child))
                  (expect (unregister-export-link-type child "custom")
                          :to-be child)
                  (setf child-handler-registered-p nil)
                  (multiple-value-bind (handler owner)
                      (org-kit::export-resolved-link-handler
                       (org-kit::export-backend-ancestry child) "CUSTOM")
                    (expect (eq handler #'export-registry-test-link-handler)
                            :to-be t)
                    (expect owner :to-be parent))
                  (expect
                   (org-export-error-reason
                    (export-registry-test-registration-error
                     (lambda () (unregister-export-link-type child "custom"))))
                   :to-be :unknown-link-type-handler)
                  (expect
                   (org-export-error-reason
                    (export-registry-test-registration-error
                     (lambda ()
                       (register-export-link-type
                        parent "CUSTOM" #'export-registry-test-link-handler))))
                   :to-be :duplicate-link-type-handler)
                  (expect (unregister-export-link-type parent "custom")
                          :to-be parent)
                  (setf parent-handler-registered-p nil)
                  (expect
                   (org-export-error-reason
                    (export-registry-test-registration-error
                     (lambda ()
                       (unregister-export-link-type parent "custom"))))
                   :to-be :unknown-link-type-handler)))
           (when child-handler-registered-p
             (unregister-export-link-type child "custom"))
           (when parent-handler-registered-p
             (unregister-export-link-type parent "custom")))))))
  (it "dispatches registered link types on the root built-in backend"
    (let ((registered-p nil)
          (output nil))
      (unwind-protect
           (progn
             (register-export-link-type
              :latex "fr106-root"
              (function export-registry-test-link-handler))
             (setf registered-p t)
             (setf output
                   (export-org
                    (parse-org "[[fr106-root:path][Root link]]")
                    :latex))
             (with-soft-assertions
               (expect
                (search "\\href{https://example.test/}{Root link}" output)
                :to-be-truthy)))
        (when registered-p
          (unregister-export-link-type :latex "fr106-root")))))

  (it "keeps built-in link registration idempotent across reloads"
    (let* ((backend :api-registry-builtin-link)
           (registry org-kit::*export-backend-registry*)
           (builtin-handlers org-kit::*export-builtin-link-type-handlers*)
           (old-descriptor nil)
           (old-builtin-types nil)
           (had-descriptor-p nil)
           (had-builtin-types-p nil)
           (handler #'export-registry-test-link-handler))
      (sb-thread:with-mutex (org-kit::*export-backend-registry-lock*)
        (multiple-value-setq (old-descriptor had-descriptor-p)
          (gethash backend registry))
        (multiple-value-setq (old-builtin-types had-builtin-types-p)
          (gethash backend builtin-handlers)))
      (unwind-protect
           (progn
             (org-kit::%register-builtin-export-backend
              backend :renderer #'identity)
             (org-kit::%register-builtin-export-link-type
              backend "custom" handler)
             (org-kit::%register-builtin-export-backend
              backend :renderer #'identity)
             (org-kit::%register-builtin-export-link-type
              backend "CUSTOM" handler)
             (multiple-value-bind (resolved owner)
                 (org-kit::export-resolved-link-handler
                  (org-kit::export-backend-ancestry backend) "Custom")
               (with-soft-assertions
                 (expect (eq resolved handler) :to-be t)
                 (expect owner :to-be backend)
                 (expect
                  (length
                   (org-kit::%export-backend-descriptor-link-type-handlers
                    (org-kit::export-registry-descriptor backend)))
                  :to-be 1)
                 (expect
                  (org-export-error-reason
                   (export-registry-test-registration-error
                    (lambda ()
                      (unregister-export-link-type backend "custom"))))
                  :to-be :builtin-link-type-handler))))
        (sb-thread:with-mutex (org-kit::*export-backend-registry-lock*)
          (if had-descriptor-p
              (setf (gethash backend registry) old-descriptor)
              (remhash backend registry))
          (if had-builtin-types-p
              (setf (gethash backend builtin-handlers) old-builtin-types)
              (remhash backend builtin-handlers))))))

  (it "copies extension and view list structure while retaining extension values"
    (let* ((value (list :shared-state))
           (extensions (list (cons :processor value)))
           (context (make-export-context :extensions extensions))
           (readback (export-context-extensions context))
           (original-child
             (make-export-view (make-node :paragraph)))
           (replacement-child
             (make-export-view (make-node :plain-text)))
           (root-node (make-node :section))
           (children (list original-child))
           (view (make-export-view root-node :children children))
           (view-children (export-view-children view)))
      (setf (caar extensions) :changed-before-read)
      (with-soft-assertions
        (expect (caar readback) :to-be :processor)
        (expect (eq (cdar readback) value) :to-be t)
        (setf (caar readback) :changed-after-read)
        (expect (caar (export-context-extensions context)) :to-be :processor)
        (setf (car children) replacement-child)
        (expect (eq (car (export-view-children view)) original-child) :to-be t)
        (setf (car view-children) replacement-child)
        (expect (eq (car (export-view-children view)) original-child) :to-be t)
        (expect (eq (export-view-node view) root-node) :to-be t)))


)
  (it "checks native component and filter commits with per-kind output-check indices"
  (labels
      ((run-case (backend arguments limit)
         (export-registry-test-with-root-backend
          backend arguments
          (lambda ()
            (multiple-value-bind (condition events)
                (export-registry-test-capture-output-check
                 (lambda ()
                   (export-org
                    (parse-org "Accounting boundaries."
                               :limits (make-limits
                                        :max-output-length limit))
                    backend)))
              (list condition (first events))))))
       (components (preamble body trailer)
         (list :preamble-renderer
               (lambda (context)
                 (declare (ignore context))
                 preamble)
               :renderer
               (lambda (context)
                 (declare (ignore context))
                 body)
               :trailer-renderer
               (lambda (context)
                 (declare (ignore context))
                 trailer)))
       (check-event (result phase kind index actual)
         (let ((condition (first result))
               (event (second result)))
           (expect (typep condition 'org-resource-limit-error) :to-be t)
           (progn
             (expect (not (null event)) :to-be t)
             (expect (getf event :phase) :to-be phase)
             (expect (getf event :kind) :to-be kind)
             (expect (getf event :index) :to-be index)
             (expect (getf event :actual) :to-be actual)))))
    (with-soft-assertions
      (check-event
       (run-case :api-registry-output-preamble
                 (components "PP" "B" "T") 1)
       :preamble :native-write 1 2)
      (check-event
       (run-case :api-registry-output-body
                 (components "P" "BB" "T") 2)
       :body :native-write 2 3)
      (check-event
       (run-case :api-registry-output-trailer
                 (components "P" "B" "TT") 3)
       :trailer :native-write 3 4)
      (check-event
       (run-case
        :api-registry-output-body-filter
        (append
         (components "P" "B" "T")
         (list :filters
               (list
                (cons :body
                      (list (lambda (value context)
                              (declare (ignore context))
                              (concatenate 'string value "!")))))))
        2)
       :body :filter-commit 1 3)
      (check-event
       (run-case
        :api-registry-output-final-filter
        (append
         (components "P" "B" "T")
         (list :filters
               (list
                (cons :final-output
                      (list (lambda (value context)
                              (declare (ignore value context))
                              "final"))))))
        3)
       :final-output :filter-commit 2 5))))
  (it "checks the template aggregate once without recounting its components"
  (let ((template-result "OUT")
        (template-components nil)
        (native-check-count nil))
    (export-registry-test-with-root-backend
     :api-registry-template-accounting
     (list :preamble-renderer
           (lambda (context)
             (declare (ignore context))
             "P")
           :renderer
           (lambda (context)
             (declare (ignore context))
             "B")
           :trailer-renderer
           (lambda (context)
             (declare (ignore context))
             "T")
           :template
           (lambda (context preamble body trailer)
             (declare (ignore context))
             (setf template-components (list preamble body trailer))
             template-result)
           :filters
           (list
            (cons :final-output
                  (list
                   (lambda (value context)
                     (declare (ignore context))
                     (setf native-check-count
                           (gethash :native-write
                                    org-kit::*export-output-check-counts*))
                     value)))))
     (lambda ()
       (let ((output
               (export-org
                (parse-org "Template accounting."
                           :limits (make-limits :max-output-length 3))
                :api-registry-template-accounting)))
         (with-soft-assertions
           (expect output :to-equal "OUT")
           (expect template-components :to-equal '("P" "B" "T"))
           (expect native-check-count :to-be 4)))
       (setf template-result "LONG")
       (multiple-value-bind (condition events)
           (export-registry-test-capture-output-check
            (lambda ()
              (export-org
               (parse-org "Template accounting."
                          :limits (make-limits :max-output-length 3))
               :api-registry-template-accounting)))
         (let ((event (first events)))
           (with-soft-assertions
             (expect (typep condition 'org-resource-limit-error) :to-be t)
             (expect (getf event :phase) :to-be :template)
             (expect (getf event :kind) :to-be :native-write)
             (expect (getf event :index) :to-be 4)
             (expect (getf event :actual) :to-be 4))))))))
  (it "sets the stream-write phase when final output writes fail"
  (let ((observed-phase nil)
        (failure nil))
    (export-registry-test-with-root-backend
     :api-registry-stream-write-phase
     (list :renderer
           (lambda (context)
             (declare (ignore context))
             "output"))
     (lambda ()
       (setf failure
      (handler-case
          (handler-bind
              ((error
                 (lambda (condition)
                   (declare (ignore condition))
                   (setf observed-phase
                         org-kit::*export-current-export-phase*))))
            (progn
              (export-org
               (parse-org "Stream write phase.")
               :api-registry-stream-write-phase
               :output-type :stream
               :stream
               (make-instance 'export-registry-test-failing-output-stream))
              nil))
        (error (condition) condition)))))
    (with-soft-assertions
      (expect (not (null failure)) :to-be t)
      (expect observed-phase :to-be :stream-write)))))

(defun export-registry-test-with-builtin-backend (name arguments function)
  (let* ((registry org-kit::*export-backend-registry*)
         (builtin-handlers org-kit::*export-builtin-link-type-handlers*)
         (old-descriptor nil)
         (old-builtin-types nil)
         (had-descriptor-p nil)
         (had-builtin-types-p nil))
    (sb-thread:with-mutex (org-kit::*export-backend-registry-lock*)
      (multiple-value-setq (old-descriptor had-descriptor-p)
        (gethash name registry))
      (multiple-value-setq (old-builtin-types had-builtin-types-p)
        (gethash name builtin-handlers)))
    (unwind-protect
         (progn
           (apply #'org-kit::%register-builtin-export-backend name arguments)
           (funcall function))
      (sb-thread:with-mutex (org-kit::*export-backend-registry-lock*)
        (if had-descriptor-p
            (setf (gethash name registry) old-descriptor)
            (remhash name registry))
        (if had-builtin-types-p
            (setf (gethash name builtin-handlers) old-builtin-types)
            (remhash name builtin-handlers))))))

(progn
  (defstruct (export-registry-test-native-sink
              (:constructor make-export-registry-test-native-sink
                  (&optional (offset 0) (newline-weight 1))))
    (text "" :type string)
    (offset 0 :type integer)
    (newline-weight 1 :type integer))

  (defun export-registry-test-native-sink-scratch (sink frame context)
    (declare (ignore frame context))
    (make-export-registry-test-native-sink
     (export-registry-test-native-sink-offset sink)
     (export-registry-test-native-sink-newline-weight sink)))

  (defun export-registry-test-native-sink-string (sink context)
    (declare (ignore context))
    (export-registry-test-native-sink-text sink))

  (defun export-registry-test-native-sink-output-offset (sink context)
    (declare (ignore context))
    (export-registry-test-native-sink-offset sink))

  (defun export-registry-test-native-sink-write-string (sink text context)
    (declare (ignore context))
    (incf (export-registry-test-native-sink-offset sink)
          (+ (length text)
             (* (1- (export-registry-test-native-sink-newline-weight sink))
                (count #\Newline text))))
    (setf (export-registry-test-native-sink-text sink)
          (concatenate 'string
                       (export-registry-test-native-sink-text sink)
                       text))
    nil)

  (defun export-registry-test-native-sink-append-node-result
      (sink text context)
    (export-registry-test-native-sink-write-string sink text context))

  (defun export-registry-test-native-sink-renderer
      (sink node view frame context)
    (declare (ignore frame))
    (if (eq (node-type node) :plain-text)
        (export-registry-test-native-sink-write-string
         sink (or (node-text node) "") context)
        (dolist (child (export-view-children view))
          (org-kit::export-render-view-to-sink context sink child)))
    nil))

(describe "native sink registry dispatch"
  (it "binds body filters to the absolute body base offset"
    (let ((observed-offset nil))
      (export-registry-test-with-root-backend
       :api-registry-filter-offset
       (list :preamble-renderer
             (lambda (context)
               (declare (ignore context))
               "P")
             :renderer
             (lambda (context)
               (declare (ignore context))
               "B")
             :filters
             (list
              (cons :body
                    (list
                     (lambda (value context)
                       (declare (ignore context))
                       (setf observed-offset
                             org-kit::*export-current-output-offset*)
                       value)))))
       (lambda ()
         (let ((output
                 (export-org (parse-org "Filter offset.")
                             :api-registry-filter-offset)))
           (with-soft-assertions
             (expect output :to-equal "PB")
             (expect observed-offset :to-be 1)))))))

  (it "applies plain-text filters before native node wrappers"
    (let ((filter-input nil)
          (filter-offset nil)
          (plain-wrapper-calls 0))
      (export-registry-test-with-builtin-backend
       :api-registry-native-filter-wrapper
       (list :renderer
             (lambda (context)
               (let ((sink (make-export-registry-test-native-sink)))
                 (dolist (view (export-render-context-root-views context))
                   (org-kit::export-render-view-to-sink context sink view))
                 (export-registry-test-native-sink-string sink nil)))
             :native-sink-renderer
             #'export-registry-test-native-sink-renderer
             :native-sink-scratch
             #'export-registry-test-native-sink-scratch
             :native-sink-string
             #'export-registry-test-native-sink-string
             :native-sink-write-string
             #'export-registry-test-native-sink-write-string
             :native-sink-append-node-result
             #'export-registry-test-native-sink-append-node-result
             :native-sink-output-offset
             #'export-registry-test-native-sink-output-offset
             :node-wrapper
             (lambda (node view frame context render-next)
               (declare (ignore view frame context))
               (if (eq (node-type node) :plain-text)
                   (progn
                     (incf plain-wrapper-calls)
                     (funcall render-next "[" "]"))
                   (funcall render-next "" ""))
               nil)
             :filters
             (list
              (cons :plain-text
                    (list
                     (lambda (value context)
                       (declare (ignore context))
                       (setf filter-input value
                             filter-offset
                             org-kit::*export-current-output-offset*)
                       "filtered")))))
       (lambda ()
         (let ((output
                 (export-org (parse-org "raw")
                             :api-registry-native-filter-wrapper)))
           (with-soft-assertions
             (expect (not (null (search "[filtered]" output))) :to-be t)
             (expect filter-input :to-equal "raw")
             (expect filter-offset :to-be 1)
             (expect plain-wrapper-calls :to-be 1)))))))

  (it "checks root wrappers against absolute native sink units without scratch"
    (let ((scratch-calls 0)
          (body-sink nil)
          (last-offset nil))
      (export-registry-test-with-builtin-backend
       :api-registry-native-wrapper-limit
       (list :renderer
             (lambda (context)
               (let ((sink
                       (make-export-registry-test-native-sink 0 2)))
                 (setf body-sink sink)
                 (dolist (view (export-render-context-root-views context))
                   (org-kit::export-render-view-to-sink context sink view))
                 (export-registry-test-native-sink-string sink nil)))
             :native-sink-renderer
             (lambda (sink node view frame context)
               (declare (ignore frame))
               (if (eq (node-type node) :plain-text)
                   (export-registry-test-native-sink-write-string
                    sink (format nil "x~%") context)
                   (dolist (child (export-view-children view))
                     (org-kit::export-render-view-to-sink
                      context sink child)))
               nil)
             :native-sink-scratch
             (lambda (sink frame context)
               (incf scratch-calls)
               (export-registry-test-native-sink-scratch
                sink frame context))
             :native-sink-string
             #'export-registry-test-native-sink-string
             :native-sink-write-string
             #'export-registry-test-native-sink-write-string
             :native-sink-append-node-result
             #'export-registry-test-native-sink-append-node-result
             :native-sink-output-offset
             (lambda (sink context)
               (declare (ignore context))
               (setf last-offset
                     (export-registry-test-native-sink-offset sink)))
             :node-wrapper
             (lambda (node view frame context render-next)
               (declare (ignore view frame context))
               (if (eq (node-type node) :plain-text)
                   (funcall render-next "[" "]")
                   (funcall render-next "" ""))
               nil))
       (lambda ()
         (multiple-value-bind (condition events)
             (export-registry-test-capture-output-check
              (lambda ()
                (export-org
                 (parse-org "raw"
                            :limits (make-limits :max-output-length 4))
                 :api-registry-native-wrapper-limit)))
           (let* ((event (first events))
                  (output (export-registry-test-native-sink-string
                           body-sink nil)))
             (with-soft-assertions
               (expect (typep condition 'org-resource-limit-error) :to-be t)
               (expect (not (null event)) :to-be t)
               (expect (getf event :phase) :to-be :body)
               (expect (getf event :kind) :to-be :native-write)
               (expect (getf event :index) :to-be 2)
               (expect (getf event :actual) :to-be last-offset)
               (expect (> last-offset (length output)) :to-be t)
               (expect (plusp (length output)) :to-be t)
               (expect scratch-calls :to-be 0)))))))))
