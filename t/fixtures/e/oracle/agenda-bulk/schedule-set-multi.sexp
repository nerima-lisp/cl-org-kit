; Generated from Org APIs with Emacs 31.1 and Org 9.8.10.
; Org oracle output is kept separate from contract policy output.
(:name "schedule-set-multi" :emacs-version
       "31.1" :org-version
       "9.8.10" :operation :agenda-bulk :action
       :schedule-set :action-key "s" :prefix
       nil :selected-headlines
       ("Weekly" "Plain")
       :completion-choice nil :date-choice "2025-03-10"
       :processed 2 :skipped 0 :source-file
       "t/fixtures/e/oracle/agenda-bulk/schedule-set-multi-source.org" :source-input
       "#+TODO: TODO | DONE
* TODO Weekly
SCHEDULED: <2024-12-02 Mon ++1w -2d>
* TODO Plain
SCHEDULED: <2024-12-03 Tue>
" :source-expected
       "#+TODO: TODO | DONE
* TODO Weekly
SCHEDULED: <2025-03-10 Mon ++1w -2d>
* TODO Plain
SCHEDULED: <2025-03-10 Mon>
" :target-input
       "" :target-heading nil
       :target-expected "" :archive-location
       nil :archive-time nil)
