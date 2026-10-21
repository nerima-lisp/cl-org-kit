; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "tag-add-multi" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :agenda-bulk :action
       :tag-add :action-key "+" :prefix nil
       :selected-headlines
       ("First" "Second")
       :completion-choice "added" :date-choice nil
       :processed 2 :skipped 0 :source-file
       "t/fixtures/e/oracle/agenda-bulk/tag-add-multi-source.org" :source-input
       "#+TAGS: old local added
* TODO First :old:
* TODO Second :local:
" :source-expected
       "#+TAGS: old local added
* TODO First                                                      :old:added:
* TODO Second                                                   :local:added:
" :target-input
       "" :target-heading nil
       :target-expected "" :archive-location
       nil :archive-time nil)
