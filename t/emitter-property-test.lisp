;;;; t/emitter-property-test.lisp
(in-package #:cl-org-kit/test)

(defconstant +p6-emitter-property-seed+ #x51A7E123)
(defconstant +p6-emitter-property-trials+ 64)
(defconstant +p6-emitter-property-mask+ #xffffffff)

(defun p6-emitter-property-random-state ()
  (vector +p6-emitter-property-seed+))

(defun p6-emitter-property-random (state bound)
  (let ((value (aref state 0)))
    (setf value
          (logand +p6-emitter-property-mask+
                  (logxor value (ash value -13))))
    (setf value
          (logand +p6-emitter-property-mask+
                  (logxor value (logand +p6-emitter-property-mask+
                                        (ash value 17)))))
    (setf value
          (logand +p6-emitter-property-mask+
                  (logxor value (ash value -5))))
    (setf (aref state 0) value)
    (mod value bound)))

(defun p6-emitter-property-hazardous-body (state)
  ;; These lines are literal data inside an example block, not Org elements.
  (format nil "* heading-like-~D~%#+TITLE: keyword-like-~D~%:END:~%payload-~D~%"
          (p6-emitter-property-random state 100000)
          (p6-emitter-property-random state 100000)
          (p6-emitter-property-random state 100000)))

(defun p6-emitter-property-headline (state level depth required-depth)
  (let* ((title (format nil "Heading-~D-~D"
                        level (p6-emitter-property-random state 100000)))
         (block-count (1+ (p6-emitter-property-random state 3)))
         (blocks (loop repeat block-count
                       collect (make-node
                                :example-block
                                :properties
                                (list :switches nil
                                      :value
                                      (p6-emitter-property-hazardous-body state)))))
         (section (make-node :section :children blocks))
         (nested-count (if (< depth required-depth)
                           (1+ (p6-emitter-property-random state 2))
                           0))
         (children
           (append (list section)
                   (loop repeat nested-count
                         collect (p6-emitter-property-headline
                                  state (1+ level) (1+ depth) required-depth)))))
    (make-node :headline
               :properties (list :level level :raw-value title)
               :children children)))

(defun p6-emitter-property-tree (state)
  (let* ((required-depth (1+ (p6-emitter-property-random state 4)))
         (headline-count (1+ (p6-emitter-property-random state 3))))
    (make-node :org-data
               :children
               (loop repeat headline-count
                     collect (p6-emitter-property-headline
                              state 1 1 required-depth)))))

(defun p6-emitter-property-signature (node)
  (list (node-type node)
        (case (node-type node)
          (:headline
           (list :level (node-property node :level)
                 :raw-value (node-property node :raw-value)))
          (:example-block
           (list :value (node-property node :value))))
        (mapcar #'p6-emitter-property-signature (node-children node))))

(defun p6-emitter-property-emit (node)
  (multiple-value-bind (symbol visibility) (find-symbol "EMIT-ORG" "ORG-KIT")
    (unless (and symbol (eq visibility :external) (fboundp symbol))
      (error "ORG-KIT:EMIT-ORG is not available as a public function"))
    (funcall symbol node)))

(defun p6-emitter-property-check-case (tree state case-index)
  (handler-case
      (let* ((expected (p6-emitter-property-signature tree))
             (rendered (p6-emitter-property-emit tree)))
        (unless (stringp rendered)
          (error "emit-org returned ~S instead of a string" rendered))
        (let* ((document (parse-org rendered))
               (actual (p6-emitter-property-signature
                        (document-root document))))
          (unless (equal expected actual)
            (error "tree mismatch; expected ~S, got ~S; emitted ~S"
                   expected actual rendered))))
    (error (condition)
      (error "P6 emit round trip failed for seed #x~X case ~D (next PRNG state #x~X): ~A"
             +p6-emitter-property-seed+ case-index (aref state 0) condition))))

(describe "P6 generated-tree emit round trips"
  (it "preserves bounded randomized headline and literal-block trees"
    (let ((state (p6-emitter-property-random-state))
          (sample-count 0))
      (dotimes (case-index +p6-emitter-property-trials+)
        (let ((tree (p6-emitter-property-tree state)))
          (p6-emitter-property-check-case tree state case-index)
          (incf sample-count)))
      (with-soft-assertions
        (expect (> sample-count 0) :to-be t)
        (expect sample-count :to-be +p6-emitter-property-trials+)))))
