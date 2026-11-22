; Generated from Org planning commands with Emacs 31.1.90 and Org 9.8.10.
; Each case replays one command on the heading titled "one".
(:name "planning-oracle" :emacs-version "31.1.90" :org-version
       "9.8.10" :cases ((:name "todo-done-scheduled" :emacs-version
                               "31.1.90" :org-version "9.8.10"
                               :source-input "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>
"
                               :operation (:todo "DONE") :timestamp
                               nil :expected "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** DONE one
CLOSED: [2026-11-22 Sun 14:20] SCHEDULED: <2026-11-21 Sat>
")
                        (:name "todo-done-deadline-and-scheduled"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** TODO one
DEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>
"
                               :operation (:todo "DONE") :timestamp
                               nil :expected "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** DONE one
CLOSED: [2026-11-22 Sun 14:20] DEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>
")
                        (:name "todo-done-bare" :emacs-version
                               "31.1.90" :org-version "9.8.10"
                               :source-input "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** TODO one
"
                               :operation (:todo "DONE") :timestamp
                               nil :expected "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** DONE one
CLOSED: [2026-11-22 Sun 14:20]
")
                        (:name "todo-done-property-drawer"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** TODO one
:PROPERTIES:
:A: b
:END:
"
                               :operation (:todo "DONE") :timestamp
                               nil :expected "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** DONE one
CLOSED: [2026-11-22 Sun 14:20]
:PROPERTIES:
:A: b
:END:
")
                        (:name "todo-done-empty-heading-blank-after"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** TODO one

** Next
"
                               :operation (:todo "DONE") :timestamp
                               nil :expected "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** DONE one
CLOSED: [2026-11-22 Sun 14:20]

** Next
")
                        (:name
                         "todo-done-scheduled-and-property-drawer"
                         :emacs-version "31.1.90" :org-version
                         "9.8.10" :source-input "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>
:PROPERTIES:
:A: b
:END:
"
                         :operation (:todo "DONE") :timestamp nil
                         :expected "#+TODO: TODO | DONE
#+STARTUP: logdone
* Cat
** DONE one
CLOSED: [2026-11-22 Sun 14:20] SCHEDULED: <2026-11-21 Sat>
:PROPERTIES:
:A: b
:END:
")
                        (:name "set-scheduled-new" :emacs-version
                               "31.1.90" :org-version "9.8.10"
                               :source-input "* Cat
** TODO one
"
                               :operation
                               (:set :scheduled "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
SCHEDULED: <2026-11-30 Mon>
")
                        (:name "set-deadline-new" :emacs-version
                               "31.1.90" :org-version "9.8.10"
                               :source-input "* Cat
** TODO one
"
                               :operation
                               (:set :deadline "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
DEADLINE: <2026-11-30 Mon>
")
                        (:name
                         "set-scheduled-empty-heading-blank-after"
                         :emacs-version "31.1.90" :org-version
                         "9.8.10" :source-input "* Cat
** TODO one

** Next
"
                         :operation (:set :scheduled "2026-11-30")
                         :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
SCHEDULED: <2026-11-30 Mon>

** Next
")
                        (:name "set-scheduled-before-deadline"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
DEADLINE: <2026-11-25 Wed>
"
                               :operation
                               (:set :scheduled "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
SCHEDULED: <2026-11-30 Mon> DEADLINE: <2026-11-25 Wed>
")
                        (:name "set-deadline-before-scheduled"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>
"
                               :operation
                               (:set :deadline "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
DEADLINE: <2026-11-30 Mon> SCHEDULED: <2026-11-21 Sat>
")
                        (:name "set-scheduled-existing-first"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat> DEADLINE: <2026-11-25 Wed>
"
                               :operation
                               (:set :scheduled "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
SCHEDULED: <2026-11-30 Mon> DEADLINE: <2026-11-25 Wed>
")
                        (:name "set-scheduled-existing-last"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
DEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>
"
                               :operation
                               (:set :scheduled "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
SCHEDULED: <2026-11-30 Mon> DEADLINE: <2026-11-25 Wed>
")
                        (:name "set-scheduled-before-property-drawer"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
:PROPERTIES:
:A: b
:END:
"
                               :operation
                               (:set :scheduled "2026-11-30")
                               :timestamp "<2026-11-30 Mon>" :expected "* Cat
** TODO one
SCHEDULED: <2026-11-30 Mon>
:PROPERTIES:
:A: b
:END:
")
                        (:name "set-closed-before-scheduled"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** DONE one
SCHEDULED: <2026-11-21 Sat>
"
                               :operation
                               (:set :closed "2026-11-20 10:00")
                               :timestamp "[2026-11-20 Fri 10:00]"
                               :expected "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-21 Sat>
")
                        (:name "set-closed-before-property-drawer"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** DONE one
:PROPERTIES:
:A: b
:END:
"
                               :operation
                               (:set :closed "2026-11-20 10:00")
                               :timestamp "[2026-11-20 Fri 10:00]"
                               :expected "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00]
:PROPERTIES:
:A: b
:END:
")
                        (:name "remove-only-scheduled-blank-after"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>

body
"
                               :operation (:remove :scheduled)
                               :timestamp nil :expected "* Cat
** TODO one

body
")
                        (:name "remove-only-deadline-blank-after"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
DEADLINE: <2026-11-25 Wed>

body
"
                               :operation (:remove :deadline)
                               :timestamp nil :expected "* Cat
** TODO one

body
")
                        (:name "remove-only-closed-blank-after"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00]

body
"
                               :operation (:remove :closed) :timestamp
                               nil :expected "* Cat
** DONE one

body
")
                        (:name "remove-only-no-blank-after"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>
body
"
                               :operation (:remove :scheduled)
                               :timestamp nil :expected "* Cat
** TODO one
body
")
                        (:name "remove-only-before-next-heading"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>

** Next
"
                               :operation (:remove :scheduled)
                               :timestamp nil :expected "* Cat
** TODO one

** Next
")
                        (:name "remove-only-before-property-drawer"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>
:PROPERTIES:
:A: b
:END:
body
"
                               :operation (:remove :scheduled)
                               :timestamp nil :expected "* Cat
** TODO one
:PROPERTIES:
:A: b
:END:
body
")
                        (:name "remove-scheduled-keep-deadline"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
DEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>

body
"
                               :operation (:remove :scheduled)
                               :timestamp nil :expected "* Cat
** TODO one
DEADLINE: <2026-11-25 Wed>

body
")
                        (:name "remove-deadline-keep-closed"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00] DEADLINE: <2026-11-25 Wed>

body
"
                               :operation (:remove :deadline)
                               :timestamp nil :expected "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00]

body
")
                        (:name
                         "remove-deadline-keep-closed-before-property-drawer"
                         :emacs-version "31.1.90" :org-version
                         "9.8.10" :source-input "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00] DEADLINE: <2026-11-25 Wed>
:PROPERTIES:
:A: b
:END:
"
                         :operation (:remove :deadline) :timestamp nil
                         :expected "* Cat
** DONE one
CLOSED: [2026-11-20 Fri 10:00]
:PROPERTIES:
:A: b
:END:
")
                        (:name "remove-closed-keep-scheduled"
                               :emacs-version "31.1.90" :org-version
                               "9.8.10" :source-input "* Cat
** TODO one
CLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-21 Sat>

body
"
                               :operation (:remove :closed) :timestamp
                               nil :expected "* Cat
** TODO one
SCHEDULED: <2026-11-21 Sat>

body
")))
