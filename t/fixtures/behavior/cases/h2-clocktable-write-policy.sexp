(:cases
 ((:id "nested-levels-default-columns"
   :source "h2-clocktable-write-nested.org"
   :total-minutes 198
   :rows ((:level 1 :title "Project" :tags ("parent") :minutes 65)
          (:level 2 :title "Tiny task" :tags ("task") :minutes 9)
          (:level 3 :title "Longer nested task" :tags ("task" "nested") :minutes 123)
          (:level 4 :title "A very long leaf headline that exceeds the default forty character width"
           :tags ("leaf") :minutes 1)))
  (:id "empty-default-table"
   :source "h2-clocktable-write-empty.org"
   :total-minutes 0
   :rows nil)
  (:id "raw-pipe-title"
   :source "h2-clocktable-write-pipe.org"
   :total-minutes 5
   :rows ((:level 1 :title "Title | pipe" :tags ("alpha" "beta") :minutes 5)))
  (:id "japanese-title-exceeds-display-narrow"
   :source "h2-clocktable-write-japanese-width.org"
   :total-minutes 5
   :rows ((:level 1 :title "日本語の表示幅を確認するための長いclocktable見出しです"
           :tags ("japanese") :minutes 5)))))
