; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "archive-parent-child" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :agenda-bulk :action
       :archive :action-key "$" :prefix nil
       :selected-headlines
       ("Parent" "Child"
				    "Other")
       :completion-choice nil :date-choice nil :processed 2 :skipped 1
       :source-file "t/fixtures/e/oracle/agenda-bulk/archive-parent-child-source.org" :source-input
       "#+TODO: TODO | DONE
* TODO Parent :PARENT:
** TODO Child :CHILD:
* TODO Other
* TODO Keep
" :source-expected
       "#+TODO: TODO | DONE
* TODO Keep
" :target-input
       "" :target-heading nil
       :target-expected "#    -*- mode: org -*-


Archived entries from file t/fixtures/e/oracle/agenda-bulk/archive-parent-child-source.org


* TODO Parent :PARENT:
:PROPERTIES:
:ARCHIVE_TIME: 2024-02-19 Mon 12:34
:ARCHIVE_FILE: t/fixtures/e/oracle/agenda-bulk/archive-parent-child-source.org
:ARCHIVE_CATEGORY: archive-parent-child-source
:ARCHIVE_TODO: TODO
:END:
** TODO Child :CHILD:

* TODO Other
:PROPERTIES:
:ARCHIVE_TIME: 2024-02-19 Mon 12:34
:ARCHIVE_FILE: t/fixtures/e/oracle/agenda-bulk/archive-parent-child-source.org
:ARCHIVE_CATEGORY: archive-parent-child-source
:ARCHIVE_TODO: TODO
:END:
" :archive-location
       "%s_archive::" :archive-time
       "2024-02-19 Mon 12:34")
