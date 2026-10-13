; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "archive-time-omitted" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :archive :source-file
       "e-oracle-context-source.org" :divergence
       :archive-time-omitted-no-time-context :additional-divergence
       :org-archive-file-header-omitted-for-caller-target
       :oracle-archive-time "2024-02-19 Mon 12:34" :source-input
       "#+FILETAGS: FILE
* Parent :PARENT:
** Child :CHILD:
*** TODO Work :LOCAL:
Body
" :target-input
       "" :source-expected
       "#+FILETAGS: FILE
* Parent :PARENT:
** Child :CHILD:
" :target-expected
       "* TODO Work                                                           :LOCAL:
:PROPERTIES:
:ARCHIVE_FILE: e-oracle-context-source.org
:ARCHIVE_OLPATH: Parent/Child
:ARCHIVE_CATEGORY: e-oracle-context-source
:ARCHIVE_TODO: TODO
:ARCHIVE_ITAGS: FILE PARENT CHILD
:END:
Body
" :oracle-target-expected
       "#    -*- mode: org -*-


Archived entries from file e-oracle-context-source.org


* TODO Work                                                           :LOCAL:
:PROPERTIES:
:ARCHIVE_TIME: 2024-02-19 Mon 12:34
:ARCHIVE_FILE: e-oracle-context-source.org
:ARCHIVE_OLPATH: Parent/Child
:ARCHIVE_CATEGORY: e-oracle-context-source
:ARCHIVE_TODO: TODO
:ARCHIVE_ITAGS: FILE PARENT CHILD
:END:
Body
")
