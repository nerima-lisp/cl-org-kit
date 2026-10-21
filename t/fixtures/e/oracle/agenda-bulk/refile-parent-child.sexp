; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "refile-parent-child" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :agenda-bulk :action
       :refile :action-key "r" :prefix nil
       :selected-headlines
       ("Parent" "Child"
				    "Other")
       :completion-choice nil :date-choice nil :processed 2 :skipped 1
       :source-file "t/fixtures/e/oracle/agenda-bulk/refile-parent-child-source.org" :source-input
       "#+TODO: TODO | DONE
* TODO Parent :PARENT:
** TODO Child :CHILD:
* TODO Other
* TODO Keep
" :source-expected
       "#+TODO: TODO | DONE
* TODO Keep
" :target-input
       "* Target
Existing
" :target-heading
       "Target" :target-expected
       "* Target
Existing
** TODO Parent                                                       :PARENT:
*** TODO Child                                                        :CHILD:
** TODO Other
" :archive-location nil
       :archive-time nil)
