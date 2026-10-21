; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "todo-multi" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :agenda-bulk :action
       :todo :action-key "t" :prefix nil
       :selected-headlines
       ("First" "Second")
       :completion-choice "DONE" :date-choice nil
       :processed 2 :skipped 0 :source-file
       "t/fixtures/e/oracle/agenda-bulk/todo-multi-source.org" :source-input
       "#+TODO: TODO | DONE
* TODO First
* TODO Second
* TODO Keep
" :source-expected
       "#+TODO: TODO | DONE
* DONE First
* DONE Second
* TODO Keep
" :target-input
       "" :target-heading nil
       :target-expected "" :archive-location
       nil :archive-time nil)
