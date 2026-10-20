;;;; src/agenda-custom.lisp
(in-package #:org-kit)

(defstruct
    (agenda-custom-view
     (:constructor %make-agenda-custom-view (key description type result))
     (:conc-name %agenda-custom-view-)
     (:copier nil)
     (:predicate %agenda-custom-view-p))
  "One custom agenda definition and its evaluated result."
  (key nil :read-only t)
  (description nil :read-only t)
  (type nil :read-only t)
  (result nil :read-only t))

(defun agenda-custom-view-p (object)
  "Return true when OBJECT is an evaluated custom agenda view."
  (%agenda-custom-view-p object))

(defun agenda-custom-view-key (view)
  "Return VIEW's definition key."
  (%agenda-custom-view-key view))

(defun agenda-custom-view-description (view)
  "Return VIEW's description, or NIL when it has none."
  (%agenda-custom-view-description view))

(defun agenda-custom-view-type (view)
  "Return VIEW's type keyword."
  (%agenda-custom-view-type view))

(defun agenda-custom-view-result (view)
  "Return VIEW's agenda result or match-entry list."
  (%agenda-custom-view-result view))

(defun agenda-custom-signal-invalid (context message)
  (agenda-signal-error :invalid-option context message))

(defun agenda-custom-plist-p (value)
  (let ((length (agenda-proper-list-length value)))
    (and (integerp length)
         (evenp length)
         (loop with seen = nil
               for tail on value by #'cddr
               for key = (car tail)
               always (and (keywordp key)
                           (not (member key seen :test #'eq))
                           (progn
                             (push key seen)
                             t))))))

(defun agenda-custom-plist-key-p (plist key)
  (loop for tail on plist by #'cddr
        thereis (eq (car tail) key)))

(defun agenda-custom-key-identity (key)
  (let ((identity
         (typecase key
           (string key)
           (symbol (symbol-name key))
           (character (string key))
           (t nil))))
    (unless (and identity (plusp (length identity)))
      (agenda-custom-signal-invalid key
                                    "Custom agenda keys must be nonempty strings, symbols, or characters"))
    identity))

(defun agenda-custom-options-keys (type)
  (case type
    (:agenda
     '(:warning-days :scheduled-delay
                     :future-repeats
                     :sorting-strategy
                     :user-defined-sorter))
    ((:todo :match :search) '(:sorting-strategy :user-defined-sorter))))

(defun agenda-custom-check-keys (plist allowed context)
  (dolist (key plist)
    (unless (member key allowed :test #'eq)
      (agenda-custom-signal-invalid context
                                    "Custom agenda definition contains an unsupported option")))
  plist)

(defun agenda-custom-normalize-definition (definition)
  (unless (agenda-custom-plist-p definition)
    (agenda-custom-signal-invalid definition
                                  "Custom agenda definitions must be property lists"))
  (agenda-custom-check-keys
   (loop for tail on definition by #'cddr
         collect (car tail))
   '(:key :description :type :start-date :end-date :match :search :options)
   definition)
  (let* ((key (getf definition :key))
         (type (getf definition :type))
         (description (getf definition :description))
         (options (getf definition :options))
         (key-identity
          (if (agenda-custom-plist-key-p definition :key)
              (agenda-custom-key-identity key)
              (agenda-custom-signal-invalid
               definition
               "Custom agenda definitions must include a key"))))
    (unless (member type '(:agenda :todo :match :search) :test #'eq)
      (agenda-custom-signal-invalid type "Unknown custom agenda view type"))
    (unless (or (null description) (stringp description))
      (agenda-custom-signal-invalid description
                                    "Custom agenda descriptions must be strings"))
    (unless (agenda-custom-plist-p options)
      (agenda-custom-signal-invalid options
                                    "Custom agenda options must be a property list"))
    (agenda-custom-check-keys
     (loop for tail on options by #'cddr
           collect (car tail))
     (agenda-custom-options-keys type)
     options)
    (case type
      (:agenda
        (unless
            (and (agenda-custom-plist-key-p definition :start-date)
                 (agenda-custom-plist-key-p definition :end-date))
          (agenda-custom-signal-invalid definition
                                        "Agenda views require start and end dates"))
        (when
            (or (agenda-custom-plist-key-p definition :match)
                (agenda-custom-plist-key-p definition :search))
          (agenda-custom-signal-invalid definition
                                        "Agenda views accept only date range arguments")))
      (:todo
       (when
           (or (agenda-custom-plist-key-p definition :start-date)
               (agenda-custom-plist-key-p definition :end-date)
               (agenda-custom-plist-key-p definition :search))
         (agenda-custom-signal-invalid definition
                                       "TODO views accept only a match argument")))
      (:match
        (unless (agenda-custom-plist-key-p definition :match)
          (agenda-custom-signal-invalid definition
                                        "Match views require a match expression"))
        (when
            (or (agenda-custom-plist-key-p definition :start-date)
                (agenda-custom-plist-key-p definition :end-date)
                (agenda-custom-plist-key-p definition :search))
          (agenda-custom-signal-invalid definition
                                        "Match views accept only a match expression")))
      (:search
        (unless (agenda-custom-plist-key-p definition :search)
          (agenda-custom-signal-invalid definition
                                        "Search views require a search string"))
        (when
            (or (agenda-custom-plist-key-p definition :start-date)
                (agenda-custom-plist-key-p definition :end-date)
                (agenda-custom-plist-key-p definition :match))
          (agenda-custom-signal-invalid definition
                                        "Search views accept only a search string"))))
    (values (copy-list definition) key-identity)))

(defun agenda-custom-normalize-definitions (definitions documents)
  (let ((length (agenda-proper-list-length definitions))
        (keys nil)
        (normalized nil))
    (unless (integerp length)
      (agenda-custom-signal-invalid definitions
                                    "Custom agenda definitions must be a proper list"))
    (check-limit
     (make-limits :max-agenda-entries
                  (minimum-limit-value (mapcar #'document-limits documents)
                                       :max-agenda-entries))
     :max-agenda-entries
     length)
    (dolist (definition definitions)
      (multiple-value-bind (copy identity)
          (agenda-custom-normalize-definition definition)
        (when (member identity keys :test #'string=)
          (agenda-custom-signal-invalid (getf definition :key)
                                        "Custom agenda view keys must be unique"))
        (push identity keys)
        (push copy normalized)))
    (nreverse normalized)))

(defun agenda-custom-evaluate-expression (value callbacks)
  (if (and (consp value) (eq (car value) :elisp))
      (let ((length (agenda-proper-list-length value)))
        (unless (and (integerp length) (= length 2))
          (agenda-custom-signal-invalid value
                                        "An ELisp expression must have the form (:elisp expression)"))
        (let ((result (evaluate-elisp callbacks (second value))))
          (unless (stringp result)
            (agenda-custom-signal-invalid value
                                          "An ELisp agenda expression must return a string"))
          result))
      value))

(defun agenda-custom-options-for (options keys)
  (loop for tail on options by #'cddr
        for key = (car tail)
        when (member key keys :test #'eq)
          append (list key (cadr tail))))

(defun agenda-custom-sort-result (result options time-context)
  (if (or (agenda-custom-plist-key-p options :sorting-strategy)
          (agenda-custom-plist-key-p options :user-defined-sorter))
      (let ((sorting-strategy (getf options :sorting-strategy))
            (user-defined-sorter (getf options :user-defined-sorter)))
        (if (agenda-result-p result)
            (make-agenda-result :entries
                                (agenda-sort-view-entries
                                 (agenda-result-entries result)
                                 sorting-strategy
                                 :user-defined-sorter
                                 user-defined-sorter
                                 :time-context
                                 time-context)
                                :start-date
                                (agenda-result-start-date result)
                                :end-date
                                (agenda-result-end-date result)
                                :time-context
                                (agenda-result-time-context result))
            (agenda-sort-view-entries result
                                      sorting-strategy
                                      :user-defined-sorter
                                      user-defined-sorter
                                      :time-context
                                      time-context)))
      result))

(defun agenda-custom-run-definition (documents definition
                                               time-context
                                               callbacks)
  (let* ((type (getf definition :type))
         (options (getf definition :options))
         (sort-options
          (agenda-custom-options-for options
                                     '(:sorting-strategy :user-defined-sorter))))
    (case type
      (:agenda
       (agenda-custom-sort-result
        (apply #'agenda
               documents
               (getf definition :start-date)
               (getf definition :end-date)
               (append (list :time-context time-context)
                       (agenda-custom-options-for options
                                                  '(:warning-days
                                                    :scheduled-delay
                                                    :future-repeats))))
        options
        time-context))
      (:todo
       (agenda-custom-sort-result
        (agenda-todo-list documents
                          :match
                          (agenda-custom-evaluate-expression
                           (getf definition :match)
                           callbacks)
                          :time-context
                          time-context)
        options
        time-context))
      (:match
       (agenda-custom-sort-result
        (agenda-match documents
                      (agenda-custom-evaluate-expression
                       (getf definition :match)
                       callbacks)
                      :time-context
                      time-context)
        options
        time-context))
      (:search
       (apply #'agenda-search-view
              documents
              (agenda-custom-evaluate-expression (getf definition :search)
                                                 callbacks)
              (append (list :time-context time-context) sort-options))))))

(defun agenda-custom (documents definitions &key time-context callbacks)
  "Evaluate ordered agenda view definitions without changing their result types."
  (let* ((documents (agenda-normalize-documents documents))
         (definitions
          (agenda-custom-normalize-definitions definitions documents))
         (callbacks (or callbacks (make-callback-registry))))
    (unless (callback-registry-p callbacks)
      (error 'type-error :datum callbacks :expected-type 'callback-registry))
    (mapcar
     (lambda (definition)
       (%make-agenda-custom-view (getf definition :key)
                                 (getf definition :description)
                                 (getf definition :type)
                                 (agenda-custom-run-definition documents
                                                               definition
                                                               time-context
                                                               callbacks)))
     definitions)))
