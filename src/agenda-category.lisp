;;;; src/agenda-category.lisp
(in-package #:org-kit)

(defun agenda-heading-category (document heading)
  "Return the effective Org category for HEADING in DOCUMENT."
  (org-effective-category document heading :fallback "???"))
