;;; Run with `nix develop --command sbcl --script .fr106-focused-test.lisp`.
;;; The export baseline check compares the full suite to a saved manifest.
;;; This script runs named cases with the original runner, without that comparison.
(require :asdf)
(asdf:load-system "cl-org-kit/test")
(dolist (filter
         '("checks HTML escaping before allocating a large unescaped run"
           "E3 GFM export > rejects an unsafe large GFM abbreviation before copying its path"
           "ASCII export > checks ASCII link output before allocating a large destination"))
  (unless
      (funcall cl-org-kit/test::*export-baseline-original-run-all*
               :reporter :spec :name-filter filter :pass-with-no-tests nil)
    (uiop:quit 1)))
(uiop:quit 0)
