((:id "column-insert-first" :source "formula-table.org"
  :action :insert-column :index 1
  :values ("Key" "new-alpha" "new-beta" "new-gamma"))
 (:id "column-insert-middle" :source "formula-table.org"
  :action :insert-column :index 2
  :values ("new" "x" "y" "z"))
 (:id "column-insert-after-last" :source "formula-table.org"
  :action :insert-column :index 4
  :values ("new" "x" "y" "z"))
 (:id "column-delete-first" :source "formula-table.org"
  :action :delete-column :index 1)
 (:id "column-delete-middle" :source "formula-table.org"
  :action :delete-column :index 2)
 (:id "column-delete-last" :source "formula-table.org"
  :action :delete-column :index 3)
 (:id "column-move-first-right" :source "formula-table.org"
  :action :move-column :from 1 :to 2)
 (:id "column-move-right" :source "formula-table.org"
  :action :move-column :from 2 :to 3)
 (:id "column-move-left" :source "formula-table.org"
  :action :move-column :from 2 :to 1)
 (:id "column-move-nonadjacent" :source "formula-table.org"
  :action :move-column :from 1 :to 3)
 (:id "row-insert-first" :source "formula-table.org"
  :action :insert-row :index 1
  :values ("new-header" "seven" "eight"))
 (:id "row-insert-middle" :source "formula-table.org"
  :action :insert-row :index 3
  :values ("new" "seven" "eight"))
 (:id "row-insert-after-last" :source "formula-table.org"
  :action :insert-row :index 5
  :values ("new" "seven" "eight"))
 (:id "row-insert-empty-values" :source "formula-table.org"
  :action :insert-row :index 2 :values nil)
 (:id "row-insert-short-values" :source "formula-table.org"
  :action :insert-row :index 2 :values ("brief"))
 (:id "row-delete-middle" :source "formula-table.org"
  :action :delete-row :index 3)
 (:id "row-delete-last" :source "formula-table.org"
  :action :delete-row :index 4)
 (:id "row-move-down" :source "formula-table.org"
  :action :move-row :from 2 :to 3)
 (:id "row-move-up" :source "formula-table.org"
  :action :move-row :from 3 :to 2)
 (:id "row-move-to-last" :source "formula-table.org"
  :action :move-row :from 3 :to 4)
 (:id "row-move-from-last" :source "formula-table.org"
  :action :move-row :from 4 :to 3)
 (:id "section-row-move" :source "sections.org"
  :action :move-row :from 2 :to 3)
 (:id "section-row-insert" :source "sections.org"
  :action :insert-row :index 4 :values ("new" "added"))
 (:id "section-row-delete" :source "sections.org"
  :action :delete-row :index 3)
 (:id "ragged-column-insert" :source "ragged.org"
  :action :insert-column :index 2
  :values ("inserted" "x" "y" "z"))
 (:id "row-delete-sole" :source "sole-row.org"
  :action :delete-row :index 1)
 (:id "column-delete-sole" :source "sole-column.org"
  :action :delete-column :index 1))
