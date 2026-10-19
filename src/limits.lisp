;;;; src/limits.lisp
(in-package #:org-kit)

(defstruct (limits (:constructor %make-limits) (:conc-name %limits-) (:copier nil)
                   (:predicate %limits-p))
  "Resource ceilings for parsing and writing. Build one with MAKE-LIMITS."
  (max-input-length 0 :read-only t)
  (max-heading-depth 0 :read-only t)
  (max-element-depth 0 :read-only t)
  (max-search-regex-length 0 :read-only t)
  (max-search-work 0 :read-only t)
  (max-object-depth 0 :read-only t)
  (max-node-count 0 :read-only t)
  (max-output-length 0 :read-only t)
  (max-output-octets 0 :read-only t)
  (max-agenda-range-days 0 :read-only t)
  (max-agenda-entries 0 :read-only t)
  (max-future-repeats 0 :read-only t)
  (max-export-expansion-depth 0 :read-only t)
  (max-export-expanded-characters 0 :read-only t)
  (max-export-expanded-nodes 0 :read-only t)
  (max-formula-tokens 0 :read-only t)
  (max-formula-ast-depth 0 :read-only t)
  (max-formula-precision 0 :read-only t)
  (max-formula-exponent 0 :read-only t)
  (max-formula-work 0 :read-only t)
  (max-write-plan-entries 0 :read-only t)
  (max-write-plan-bytes 0 :read-only t)
  (max-write-plan-path-bytes 0 :read-only t)
  (max-write-plan-path-depth 0 :read-only t))

(defun checked-limit-value (value)
  (unless (typep value '(integer 0))
    (error 'type-error :datum value :expected-type '(integer 0)))
  value)

(defun make-limits (&key (max-input-length (* 64 1024 1024))
                         (max-heading-depth 256)
                         (max-element-depth 512)
                         (max-search-regex-length 4096)
                         (max-search-work (* 64 1024 1024))
                         (max-object-depth 128)
                         (max-node-count (* 4 1024 1024))
                         (max-output-length (* 64 1024 1024))
                         (max-output-octets (* 256 1024 1024))
                         (max-agenda-range-days 3660)
                         (max-agenda-entries 1000000)
                         (max-future-repeats 10000)
                         (max-export-expansion-depth 32)
                         (max-export-expanded-characters (* 64 1024 1024))
                         (max-export-expanded-nodes (* 4 1024 1024))
                         (max-formula-tokens 10000)
                         (max-formula-ast-depth 256)
                         (max-formula-precision 34)
                         (max-formula-exponent 6144)
                         (max-formula-work 1000000)
                         (max-write-plan-entries 100000)
                         (max-write-plan-bytes (* 256 1024 1024))
                         (max-write-plan-path-bytes 4096)
                         (max-write-plan-path-depth 128))
  "Return a LIMITS object whose values are non-negative integers.

Input length counts octets or decoded characters; output length counts Unicode
characters or encoded octets. Node counts and heading, element, and object
depths are measured per document. Search regex length is measured in characters.
Regex search work counts candidate text characters cumulatively per searched
document and link-resolution index. Agenda entries and repeats count generated
results per agenda call, and agenda ranges use inclusive days. Export character
and node budgets count cumulative expansion per export call, with the root at
depth zero. Formula token, AST-depth, precision, and exponent limits are per
formula; work is per table recalculation. Write-plan entries count files in one
application, and write-plan bytes count combined UTF-8 string content and
octet-vector content. Write-plan path bytes count UTF-8 octets in each submitted
path, and path depth counts non-empty components other than dot. Each keyword
overrides its corresponding default with a non-negative integer."
  (%make-limits :max-input-length (checked-limit-value max-input-length)
                :max-heading-depth (checked-limit-value max-heading-depth)
                :max-element-depth (checked-limit-value max-element-depth)
                :max-search-regex-length
                (checked-limit-value max-search-regex-length)
                :max-search-work (checked-limit-value max-search-work)
                :max-object-depth (checked-limit-value max-object-depth)
                :max-node-count (checked-limit-value max-node-count)
                :max-output-length (checked-limit-value max-output-length)
                :max-output-octets (checked-limit-value max-output-octets)
                :max-agenda-range-days (checked-limit-value max-agenda-range-days)
                :max-agenda-entries (checked-limit-value max-agenda-entries)
                :max-future-repeats (checked-limit-value max-future-repeats)
                :max-export-expansion-depth
                (checked-limit-value max-export-expansion-depth)
                :max-export-expanded-characters
                (checked-limit-value max-export-expanded-characters)
                :max-export-expanded-nodes
                (checked-limit-value max-export-expanded-nodes)
                :max-formula-tokens (checked-limit-value max-formula-tokens)
                :max-formula-ast-depth (checked-limit-value max-formula-ast-depth)
                :max-formula-precision (checked-limit-value max-formula-precision)
                :max-formula-exponent (checked-limit-value max-formula-exponent)
                :max-formula-work (checked-limit-value max-formula-work)
                :max-write-plan-entries
                (checked-limit-value max-write-plan-entries)
                :max-write-plan-bytes (checked-limit-value max-write-plan-bytes)
                :max-write-plan-path-bytes
                (checked-limit-value max-write-plan-path-bytes)
                :max-write-plan-path-depth
                (checked-limit-value max-write-plan-path-depth)))

(defun limits-p (object)
  "Return true when OBJECT is a limit configuration returned by MAKE-LIMITS."
  (%limits-p object))

(defun limit-value (limits name)
  "Return the value of the limit NAME, one of the MAKE-LIMITS keywords, in LIMITS."
  (ecase name
    (:max-input-length (%limits-max-input-length limits))
    (:max-heading-depth (%limits-max-heading-depth limits))
    (:max-element-depth (%limits-max-element-depth limits))
    (:max-search-regex-length (%limits-max-search-regex-length limits))
    (:max-search-work (%limits-max-search-work limits))
    (:max-object-depth (%limits-max-object-depth limits))
    (:max-node-count (%limits-max-node-count limits))
    (:max-output-length (%limits-max-output-length limits))
    (:max-output-octets (%limits-max-output-octets limits))
    (:max-agenda-range-days (%limits-max-agenda-range-days limits))
    (:max-agenda-entries (%limits-max-agenda-entries limits))
    (:max-future-repeats (%limits-max-future-repeats limits))
    (:max-export-expansion-depth (%limits-max-export-expansion-depth limits))
    (:max-export-expanded-characters
     (%limits-max-export-expanded-characters limits))
    (:max-export-expanded-nodes (%limits-max-export-expanded-nodes limits))
    (:max-formula-tokens (%limits-max-formula-tokens limits))
    (:max-formula-ast-depth (%limits-max-formula-ast-depth limits))
    (:max-formula-precision (%limits-max-formula-precision limits))
    (:max-formula-exponent (%limits-max-formula-exponent limits))
    (:max-formula-work (%limits-max-formula-work limits))
    (:max-write-plan-entries (%limits-max-write-plan-entries limits))
    (:max-write-plan-bytes (%limits-max-write-plan-bytes limits))
    (:max-write-plan-path-bytes (%limits-max-write-plan-path-bytes limits))
    (:max-write-plan-path-depth (%limits-max-write-plan-path-depth limits))))

(defun minimum-limit-value (limits name)
  "Return the minimum value of NAME in LIMITS, or its default when LIMITS is empty.
LIMITS is a list of limits objects, typically collected from DOCUMENT-LIMITS."
  (if limits
      (reduce #'min limits :key (lambda (configured-limits)
                                  (limit-value configured-limits name)))
      (limit-value (make-limits) name)))

(defun check-limit (limits name actual)
  "Signal ORG-RESOURCE-LIMIT-ERROR when ACTUAL exceeds limit NAME in LIMITS;
otherwise return ACTUAL."
  (let ((limit (limit-value limits name)))
    (when (> actual limit)
      (error 'org-resource-limit-error
             :message "Resource limit exceeded"
             :limit-name name :limit limit :actual actual))
    actual))
