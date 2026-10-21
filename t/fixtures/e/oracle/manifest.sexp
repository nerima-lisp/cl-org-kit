; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "e-oracle-manifest" :emacs-version
       "31.1" :org-version
       "9.8.10" :cases
       ((:name "alpha" :path
	       "sort/alpha.sexp" :key :alpha)
	(:name "numeric" :path
	       "sort/numeric.sexp" :key :numeric)
	(:name "time" :path
	       "sort/time.sexp" :key :time)
	(:name "created" :path
	       "sort/created.sexp" :key :created)
	(:name "scheduled" :path
	       "sort/scheduled.sexp" :key :scheduled)
	(:name "deadline" :path
	       "sort/deadline.sexp" :key :deadline)
	(:name "priority" :path
	       "sort/priority.sexp" :key :priority)
	(:name "todo" :path
	       "sort/todo.sexp" :key :todo)
	(:name "property" :path
	       "sort/property.sexp" :key :property)
	(:name "priority-directive" :path
	       "sort/priority-directive.sexp" :key :priority)
	(:name "refile-root" :path
	       "relocate/refile-root.sexp" :key :refile)
	(:name "refile-heading" :path
	       "relocate/refile-heading.sexp" :key :refile)
	(:name "refile-incompatible-todo" :path
	       "relocate/refile-incompatible-todo.sexp" :key :refile)
	(:name "archive-root" :path
	       "archive/archive-root.sexp" :key :archive)
	(:name "archive-heading" :path
	       "archive/archive-heading.sexp" :key :archive)
	(:name "archive-context" :path
	       "archive/archive-context.sexp" :key :archive)
	(:name "archive-time-omitted" :path
	       "../policy/archive/archive-time-omitted.sexp" :key :archive :policy t)
	(:name "property-missing-last" :path
	       "../policy/sort/property-missing-last.sexp" :key :property :policy t)
	(:name "created-missing-last" :path
	       "../policy/sort/created-missing-last.sexp" :key :created :policy t)
	(:name "todo-missing-last" :path
	       "../policy/sort/todo-missing-last.sexp" :key :todo :policy t)
	(:name "alpha-code-point-case-sensitive" :path
	       "../policy/sort/alpha-code-point-case-sensitive.sexp" :key :alpha :policy t)
	(:name "todo-multi" :path
	       "agenda-bulk/todo-multi.sexp" :key :agenda-bulk)
	(:name "tag-add-multi" :path
	       "agenda-bulk/tag-add-multi.sexp" :key :agenda-bulk)
	(:name "schedule-set-multi" :path
	       "agenda-bulk/schedule-set-multi.sexp" :key :agenda-bulk)
	(:name "schedule-delete-multi" :path
	       "agenda-bulk/schedule-delete-multi.sexp" :key :agenda-bulk)
	(:name "refile-parent-child" :path
	       "agenda-bulk/refile-parent-child.sexp" :key :agenda-bulk)
	(:name "archive-parent-child" :path
	       "agenda-bulk/archive-parent-child.sexp" :key :agenda-bulk)))
