; Generated from Org agenda APIs with Emacs 31.1 and Org 9.8.10.
; Fixed reference times and UTC calendar dates come from the case data.
; Agenda and TODO rows use Org APIs; the fixture's diary sexp is not evaluated.
(:source "agenda.org" :emacs-version "31.1" :org-version "9.8.10"
	 :snapshots
	 ((:name :warning-d4 :reference-time
		 "2025-03-03T12:00:00+00:00" :start-date "2025-03-03"
		 :end-date "2025-03-03" :warning-days 14
		 :scheduled-delay 2 :future-repeats :all :entries
		 ((:date "2025-03-03" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-03"
			 :timestamp "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-03" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-03"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-03" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :due :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-03" :kind :timestamp :org-type
			 "block" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :warning-d3 :reference-time
		 "2025-03-04T12:00:00+00:00" :start-date "2025-03-04"
		 :end-date "2025-03-04" :warning-days 14
		 :scheduled-delay 2 :future-repeats :all :entries
		 ((:date "2025-03-04" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-04"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-04" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-04"
			 :timestamp "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-04" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-04"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-04" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-04"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-04" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-04" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-04" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-04" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-04" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-04" :kind :timestamp :org-type
			 "block" :org-date "2025-03-04" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :warning-d1 :reference-time
		 "2025-03-06T12:00:00+00:00" :start-date "2025-03-06"
		 :end-date "2025-03-06" :warning-days 14
		 :scheduled-delay 2 :future-repeats :all :entries
		 ((:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :due :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-06" :kind :timestamp :org-type
			 "block" :org-date "2025-03-06" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :warning-d0 :reference-time
		 "2025-03-07T12:00:00+00:00" :start-date "2025-03-07"
		 :end-date "2025-03-07" :warning-days 14
		 :scheduled-delay 2 :future-repeats :all :entries
		 ((:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Deadline warning -3d"
			 :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :overdue :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-07"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-07"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Completed deadline" :heading-path
			 ("Parent" "Completed deadline")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "DONE"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 51 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Child property override" :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "block" :org-date "2025-03-07" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active single date" :heading-path
			 ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Global property fallback" :heading-path
			 ("Global property fallback") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags ("filetag")
			 :property-values (("REGION" . "global"))
			 :habit-p nil :source-line 54 :match-p nil)))
	  (:name :warning-after-due :reference-time
		 "2025-03-08T12:00:00+00:00" :start-date "2025-03-08"
		 :end-date "2025-03-08" :warning-days 14
		 :scheduled-delay 0 :future-repeats :all :entries
		 ((:date "2025-03-08" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Deadline warning -3d"
			 :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :overdue :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-08" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :overdue :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-08" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-08"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-08" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-08"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-08" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-07"
			 :timestamp "<2025-03-07 Fri .+1d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp "<2025-03-07 Fri .+1d>"
			 :deadline-timestamp nil :headline
			 "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-08" :kind :timestamp :org-type
			 "block" :org-date "2025-03-08" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :scheduled-delay-d2 :reference-time
		 "2025-03-05T12:00:00+00:00" :start-date "2025-03-05"
		 :end-date "2025-03-05" :warning-days 14
		 :scheduled-delay 2 :future-repeats :all :entries
		 ((:date "2025-03-05" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-05"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-05" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-05"
			 :timestamp "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-05" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-05"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-05" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-05"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-05" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 2 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-05" :kind :timestamp :org-type
			 "block" :org-date "2025-03-05" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :repeating-warning-prewarning :reference-time
		 "2025-03-02T12:00:00+00:00" :start-date "2025-03-02"
		 :end-date "2025-03-02" :warning-days 14
		 :scheduled-delay 0 :future-repeats :all :entries
		 ((:date "2025-03-02" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-02"
			 :timestamp "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-02" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-02"
			 :timestamp "<2025-03-03 Mon ++2d -1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :prewarning :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-02" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-02" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-02" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)))
	  (:name :repeating-delay-cycle :reference-time
		 "2025-03-03T12:00:00+00:00" :start-date "2025-03-03"
		 :end-date "2025-03-06" :warning-days 14
		 :scheduled-delay 0 :future-repeats :all :entries
		 ((:date "2025-03-03" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-03"
			 :timestamp "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-03" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-03"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-03" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :due :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon +2d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon +2d>" :deadline-timestamp
			 nil :headline "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon ++2d>" :deadline-timestamp
			 nil :headline "Repeater restart"
			 :heading-path ("Parent" "Repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon .+2d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon .+2d>" :deadline-timestamp
			 nil :headline "Repeater from now"
			 :heading-path ("Parent" "Repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-03" :kind :timestamp :org-type
			 "block" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-04" :kind :timestamp :org-type
			 "block" :org-date "2025-03-04" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-05" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-05" :timestamp
			 "<2025-03-03 Mon ++2d --1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-05" :timestamp
			 "<2025-03-03 Mon +2d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon +2d>" :deadline-timestamp
			 nil :headline "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-05" :timestamp
			 "<2025-03-03 Mon ++2d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon ++2d>" :deadline-timestamp
			 nil :headline "Repeater restart"
			 :heading-path ("Parent" "Repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-05" :timestamp
			 "<2025-03-03 Mon .+2d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp
			 "<2025-03-03 Mon .+2d>" :deadline-timestamp
			 nil :headline "Repeater from now"
			 :heading-path ("Parent" "Repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-05" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-05" :kind :timestamp :org-type
			 "block" :org-date "2025-03-05" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :due :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-06" :kind :timestamp :org-type
			 "block" :org-date "2025-03-06" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :repeating-warning-and-delay :reference-time
		 "2025-03-06T12:00:00+00:00" :start-date "2025-03-03"
		 :end-date "2025-03-10" :warning-days 14
		 :scheduled-delay 0 :future-repeats :all :entries
		 ((:date "2025-03-03" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :due :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-03" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-03" :kind :timestamp :org-type
			 "block" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-04" :kind :timestamp :org-type
			 "block" :org-date "2025-03-04" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-05" :kind :timestamp :org-type
			 "block" :org-date "2025-03-05" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :due :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-06" :kind :timestamp :org-type
			 "block" :org-date "2025-03-06" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Deadline warning -3d"
			 :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Completed deadline" :heading-path
			 ("Parent" "Completed deadline")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "DONE"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 51 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Child property override" :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "block" :org-date "2025-03-07" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active single date" :heading-path
			 ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Global property fallback" :heading-path
			 ("Global property fallback") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags ("filetag")
			 :property-values (("REGION" . "global"))
			 :habit-p nil :source-line 54 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-08" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-08" :kind :timestamp :org-type
			 "block" :org-date "2025-03-08" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-09" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-09" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-09" :kind :timestamp :org-type
			 "block" :org-date "2025-03-09" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-10" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-10" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-10" :kind :timestamp :org-type
			 "block" :org-date "2025-03-10" :timestamp
			 "<2025-03-10 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :repeaters-all :reference-time
		 "2025-03-06T12:00:00+00:00" :start-date "2025-03-06"
		 :end-date "2025-03-10" :warning-days 14
		 :scheduled-delay 0 :future-repeats :all :entries
		 ((:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :due :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-06" :kind :timestamp :org-type
			 "block" :org-date "2025-03-06" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Deadline warning -3d"
			 :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Completed deadline" :heading-path
			 ("Parent" "Completed deadline")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "DONE"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 51 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Child property override" :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "block" :org-date "2025-03-07" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active single date" :heading-path
			 ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Global property fallback" :heading-path
			 ("Global property fallback") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags ("filetag")
			 :property-values (("REGION" . "global"))
			 :habit-p nil :source-line 54 :match-p nil)
		  (:date "2025-03-08" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-08" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-08" :kind :timestamp :org-type
			 "block" :org-date "2025-03-08" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-09" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-09" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-09" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-09" :kind :timestamp :org-type
			 "block" :org-date "2025-03-09" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-10" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-10" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-10" :kind :timestamp :org-type
			 "block" :org-date "2025-03-10" :timestamp
			 "<2025-03-10 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :repeaters-next :reference-time
		 "2025-03-06T12:00:00+00:00" :start-date "2025-03-06"
		 :end-date "2025-03-10" :warning-days 14
		 :scheduled-delay 0 :future-repeats :next :entries
		 ((:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :due :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-06" :kind :timestamp :org-type
			 "block" :org-date "2025-03-06" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Deadline warning -3d"
			 :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Completed deadline" :heading-path
			 ("Parent" "Completed deadline")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "DONE"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 51 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Child property override" :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "block" :org-date "2025-03-07" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active single date" :heading-path
			 ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Global property fallback" :heading-path
			 ("Global property fallback") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags ("filetag")
			 :property-values (("REGION" . "global"))
			 :habit-p nil :source-line 54 :match-p nil)
		  (:date "2025-03-08" :kind :timestamp :org-type
			 "block" :org-date "2025-03-08" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-09" :kind :timestamp :org-type
			 "block" :org-date "2025-03-09" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-10" :kind :timestamp :org-type
			 "block" :org-date "2025-03-10" :timestamp
			 "<2025-03-10 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)))
	  (:name :repeaters-none :reference-time
		 "2025-03-06T12:00:00+00:00" :start-date "2025-03-06"
		 :end-date "2025-03-10" :warning-days 14
		 :scheduled-delay 0 :future-repeats :none :entries
		 ((:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-07 Fri -3d>"
			 :timestamp-date "2025-03-07"
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :prewarning :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-06" :timestamp
			 "<2025-03-06 Thu>" :timestamp-date
			 "2025-03-06" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-06 Thu>"
			 :headline "Deadline default warning"
			 :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state :due :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-17 Mon>" :timestamp-date
			 "2025-03-17" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-17 Mon>"
			 :headline "Deadline default D-14"
			 :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "upcoming-deadline" :org-date "2025-03-06"
			 :timestamp "<2025-03-18 Tue>" :timestamp-date
			 "2025-03-18" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-18 Tue>"
			 :headline "Deadline default D-15"
			 :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state :prewarning :warning-days 14
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date "2025-03-06" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-03" :timestamp
			 "<2025-03-03 Mon ++2d -1d>" :timestamp-date
			 "2025-03-03" :scheduled-timestamp nil
			 :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state :overdue :warning-days 1
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon --2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 :delayed :scheduled-delay-days 2 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d --1d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state :delayed
			 :scheduled-delay-days 1 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon +2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon ++2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-03-03"
			 :timestamp "<2025-03-03 Mon .+2d>"
			 :timestamp-date "2025-03-03"
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat +2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat ++2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date "2025-03-06" :kind :scheduled :org-type
			 "past-scheduled" :org-date "2025-02-01"
			 :timestamp "<2025-02-01 Sat .+2d>"
			 :timestamp-date "2025-02-01"
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days 0 :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date "2025-03-06" :kind :timestamp :org-type
			 "block" :org-date "2025-03-06" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Deadline warning -3d"
			 :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date "2025-03-07" :kind :deadline :org-type
			 "deadline" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri -3d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp "<2025-03-07 Fri -3d>"
			 :headline "Completed deadline" :heading-path
			 ("Parent" "Completed deadline")
			 :warning-state :due :warning-days 3
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "DONE"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 51 :match-p nil)
		  (:date "2025-03-07" :kind :scheduled :org-type
			 "scheduled" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri .+1d>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp
			 "<2025-03-07 Fri .+1d>" :deadline-timestamp
			 nil :headline "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days 0 :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Child property override" :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "block" :org-date "2025-03-07" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active single date" :heading-path
			 ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p nil)
		  (:date "2025-03-07" :kind :timestamp :org-type
			 "timestamp" :org-date "2025-03-07" :timestamp
			 "<2025-03-07 Fri>" :timestamp-date
			 "2025-03-07" :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Global property fallback" :heading-path
			 ("Global property fallback") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags ("filetag")
			 :property-values (("REGION" . "global"))
			 :habit-p nil :source-line 54 :match-p nil)
		  (:date "2025-03-08" :kind :timestamp :org-type
			 "block" :org-date "2025-03-08" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-09" :kind :timestamp :org-type
			 "block" :org-date "2025-03-09" :timestamp nil
			 :timestamp-date nil :scheduled-timestamp nil
			 :deadline-timestamp nil :headline
			 "Active date range" :heading-path
			 ("Parent" "Active date range") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date "2025-03-10" :kind :timestamp :org-type
			 "block" :org-date "2025-03-10" :timestamp
			 "<2025-03-10 Mon>" :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil))))
	 :match-cases
	 ((:name :no-match :match "tag-that-does-not-exist" :entries
		 nil)
	  (:name :matched-todo :match "TODO=\"TODO\"" :entries
		 ((:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Child property override"
			 :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-06 Thu>" :headline
			 "Deadline default warning" :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-17 Mon>" :headline
			 "Deadline default D-14" :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-18 Tue>" :headline
			 "Deadline default D-15" :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active single date"
			 :heading-path ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Inactive single date"
			 :heading-path
			 ("Parent" "Inactive single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 44 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-07 Fri .+1d>"
			 :deadline-timestamp nil :headline
			 "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Global property fallback"
			 :heading-path ("Global property fallback")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags ("filetag") :property-values
			 (("REGION" . "global")) :habit-p nil
			 :source-line 54 :match-p t)))
	  (:name :inherited-parent-tag :match "outer" :entries
		 ((:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Parent" :heading-path
			 ("Parent") :warning-state nil :warning-days
			 nil :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state nil
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 5 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Child property override"
			 :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-06 Thu>" :headline
			 "Deadline default warning" :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-17 Mon>" :headline
			 "Deadline default D-14" :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-18 Tue>" :headline
			 "Deadline default D-15" :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active single date"
			 :heading-path ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Inactive single date"
			 :heading-path
			 ("Parent" "Inactive single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 44 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-07 Fri .+1d>"
			 :deadline-timestamp nil :headline
			 "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p t :source-line 46 :match-p t)
		  (:date nil :kind nil :org-type nil :org-date nil
			 :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Completed deadline" :heading-path
			 ("Parent" "Completed deadline")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "DONE"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 51 :match-p t))))
	 :todo-cases
	 ((:name :all-todo :match nil :entries
		 ((:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Child property override"
			 :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-06 Thu>" :headline
			 "Deadline default warning" :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-17 Mon>" :headline
			 "Deadline default D-14" :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-18 Tue>" :headline
			 "Deadline default D-15" :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active single date"
			 :heading-path ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Inactive single date"
			 :heading-path
			 ("Parent" "Inactive single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 44 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-07 Fri .+1d>"
			 :deadline-timestamp nil :headline
			 "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 46 :match-p nil)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Global property fallback"
			 :heading-path ("Global property fallback")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags ("filetag") :property-values
			 (("REGION" . "global")) :habit-p nil
			 :source-line 54 :match-p nil)))
	  (:name :no-match :match "tag-that-does-not-exist" :entries
		 nil)
	  (:name :matched-todo :match "TODO=\"TODO\"" :entries
		 ((:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Child property override"
			 :heading-path
			 ("Parent" "Child property override")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate" "local")
			 :property-values (("REGION" . "child"))
			 :habit-p nil :source-line 9 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-07 Fri -3d>" :headline
			 "Deadline warning -3d" :heading-path
			 ("Parent" "Deadline warning -3d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "local" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 14 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-06 Thu>" :headline
			 "Deadline default warning" :heading-path
			 ("Parent" "Deadline default warning")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 16 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-17 Mon>" :headline
			 "Deadline default D-14" :heading-path
			 ("Parent" "Deadline default D-14")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 18 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-18 Tue>" :headline
			 "Deadline default D-15" :heading-path
			 ("Parent" "Deadline default D-15")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 20 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 "<2025-03-03 Mon ++2d -1d>" :headline
			 "Deadline repeating warning -1d"
			 :heading-path
			 ("Parent" "Deadline repeating warning -1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 22 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon --2d>"
			 :deadline-timestamp nil :headline
			 "Scheduled delay" :heading-path
			 ("Parent" "Scheduled delay") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 24 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp
			 "<2025-03-03 Mon ++2d --1d>"
			 :deadline-timestamp nil :headline
			 "Scheduled repeating delay --1d"
			 :heading-path
			 ("Parent" "Scheduled repeating delay --1d")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 26 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon +2d>"
			 :deadline-timestamp nil :headline
			 "Repeater plus" :heading-path
			 ("Parent" "Repeater plus") :warning-state nil
			 :warning-days nil :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 28 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon ++2d>"
			 :deadline-timestamp nil :headline
			 "Repeater restart" :heading-path
			 ("Parent" "Repeater restart") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 30 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-03 Mon .+2d>"
			 :deadline-timestamp nil :headline
			 "Repeater from now" :heading-path
			 ("Parent" "Repeater from now") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 32 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat +2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater plus" :heading-path
			 ("Parent" "Stale repeater plus")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 34 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat ++2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater restart" :heading-path
			 ("Parent" "Stale repeater restart")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 36 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-02-01 Sat .+2d>"
			 :deadline-timestamp nil :headline
			 "Stale repeater from now" :heading-path
			 ("Parent" "Stale repeater from now")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 38 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active date range"
			 :heading-path ("Parent" "Active date range")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 40 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Active single date"
			 :heading-path ("Parent" "Active single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 42 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Inactive single date"
			 :heading-path
			 ("Parent" "Inactive single date")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 44 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp "<2025-03-07 Fri .+1d>"
			 :deadline-timestamp nil :headline
			 "Habit scheduled" :heading-path
			 ("Parent" "Habit scheduled") :warning-state
			 nil :warning-days nil :scheduled-delay-state
			 nil :scheduled-delay-days nil :todo-state
			 "TODO" :effective-tags
			 ("filetag" "outer" "duplicate")
			 :property-values (("REGION" . "parent"))
			 :habit-p nil :source-line 46 :match-p t)
		  (:date nil :kind :todo :org-type "todo" :org-date
			 nil :timestamp nil :timestamp-date nil
			 :scheduled-timestamp nil :deadline-timestamp
			 nil :headline "Global property fallback"
			 :heading-path ("Global property fallback")
			 :warning-state nil :warning-days nil
			 :scheduled-delay-state nil
			 :scheduled-delay-days nil :todo-state "TODO"
			 :effective-tags ("filetag") :property-values
			 (("REGION" . "global")) :habit-p nil
			 :source-line 54 :match-p t)))))
