; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "archive-heading" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :archive
       :archive-location "%s_archive::* Archive" :archive-time
       "2024-02-19 Mon 12:34" :source-file
       "e-oracle-heading-source.org" :source-input
       "* Keep
keep body
* Work
work body
" :target-input
       "" :source-expected
       "* Keep
keep body
" :target-expected
       "#    -*- mode: org -*-


Archived entries from file e-oracle-heading-source.org


* Archive

** Work
:PROPERTIES:
:ARCHIVE_TIME: 2024-02-19 Mon 12:34
:ARCHIVE_FILE: e-oracle-heading-source.org
:ARCHIVE_CATEGORY: e-oracle-heading-source
:END:
work body
")
