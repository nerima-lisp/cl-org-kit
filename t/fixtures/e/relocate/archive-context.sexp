(:name "archive-context"
 :source-file "tasks.org"
 :source-heading "Work"
 :target-heading "Archive"
 :source-input "#+CATEGORY: projects
#+FILETAGS: FILE
* Parent :PARENT:
** Child :CHILD:
*** TODO Work :LOCAL:
:PROPERTIES:
:CUSTOM: keep
:ARCHIVE_CATEGORY: old
:END:
Body
"
 :target-input "* Archive
"
 :source-expected "#+CATEGORY: projects
#+FILETAGS: FILE
* Parent :PARENT:
** Child :CHILD:
"
 :target-expected "* Archive
** TODO Work :FILE:PARENT:CHILD:LOCAL:
:PROPERTIES:
:CUSTOM: keep
:ARCHIVE_CATEGORY: projects
:ARCHIVE_FILE: tasks.org
:ARCHIVE_OLPATH: Parent/Child
:ARCHIVE_TODO: TODO
:ARCHIVE_ITAGS: FILE PARENT CHILD
:END:
Body
")
