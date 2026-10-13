(:cases
 ((:id "closed-clipping-overlap"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Closed clipping and overlap")
   :from "2026-10-12T09:00:00"
   :to "2026-10-12T11:00:00"
   :reference-time "2026-10-12T11:00:00"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "nested-subtree-clock"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Closed clipping and overlap" "Nested clock")
   :from "2026-10-12T09:00:00"
   :to "2026-10-12T12:00:00"
   :reference-time "2026-10-12T12:00:00"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "fractional-clipping"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Fractional clipping")
   :from "2026-10-12T09:00:30"
   :to "2026-10-12T09:01:30"
   :reference-time "2026-10-12T09:01:30"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "fractional-clipping-one-minute"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Fractional clipping to one minute")
   :from "2026-10-12T09:00:30"
   :to "2026-10-12T09:01:30"
   :reference-time "2026-10-12T09:01:30"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "empty-range"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Closed clipping and overlap")
   :from "2026-10-12T10:00:00"
   :to "2026-10-12T10:00:00"
   :reference-time "2026-10-12T10:00:00"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "unscoped-default"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Unscoped duration")
   :from "2026-10-12T09:00:00"
   :to "2026-10-12T10:00:00"
   :reference-time "2026-10-12T10:00:00"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "running-default-exclusion"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Running clock default exclusion")
   :active-clock-start "2026-10-12T09:00:00"
   :as-of "2026-10-12T10:00:00"
   :from "2026-10-12T09:00:00"
   :to "2026-10-12T11:00:00"
   :reference-time "2026-10-12T10:00:00"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "running-opt-in-as-of"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Running clock opt-in")
   :active-clock-start "2026-10-12T09:00:00"
   :as-of "2026-10-12T10:00:00"
   :from "2026-10-12T09:00:00"
   :to "2026-10-12T11:00:00"
   :reference-time "2026-10-12T10:00:00"
   :zone "UTC"
   :dst-policy :compatible
   :include-running-p t
   :include-unscoped-p t)
  (:id "spring-forward"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Spring-forward clock")
   :from "2026-03-08T00:00:00"
   :to "2026-03-08T23:59:00"
   :reference-time "2026-03-08T23:59:00"
   :zone "America/New_York"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "nonexistent-local-start"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Nonexistent local start")
   :from "2026-03-08T00:00:00"
   :to "2026-03-08T23:59:00"
   :reference-time "2026-03-08T23:59:00"
   :zone "America/New_York"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)
  (:id "ambiguous-local-start"
   :action :sum-clocks
   :source "g1-clock.org"
   :heading-path ("Ambiguous local start")
   :from "2026-11-01T00:00:00"
   :to "2026-11-01T23:59:00"
   :reference-time "2026-11-01T23:59:00"
   :zone "America/New_York"
   :dst-policy :compatible
   :include-running-p nil
   :include-unscoped-p t)))
