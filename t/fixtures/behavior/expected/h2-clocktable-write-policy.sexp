; Generated from org-clocktable-write-default with Emacs 31.1 and Org 9.8.10.
; Policy fixture: omit Org's volatile current-time caption for pure H2 editing.
; Formatting uses org-clocktable-defaults; all supplied rows are retained.
(:cases
 ((:id "nested-levels-default-columns" :org-clocktable-body "| Headline                                  | Time   |      |
|-------------------------------------------+--------+------|
| *Total time*                              | *3:18* |      |
|-------------------------------------------+--------+------|
| Project                                   | 1:05   |      |
| \\_  Tiny task                             |        | 0:09 |
| \\_    Longer nested task                  |        | 2:03 |
| \\_      A very long leaf headline that... |        | 0:01 |
" :org-updated-document "前文: 日本語の文字を保持します。

#+BEGIN: clocktable :scope invalid :maxlevel 1 :indent nil :narrow 1 :tags t
| Headline                                  | Time   |      |
|-------------------------------------------+--------+------|
| *Total time*                              | *3:18* |      |
|-------------------------------------------+--------+------|
| Project                                   | 1:05   |      |
| \\_  Tiny task                             |        | 0:09 |
| \\_    Longer nested task                  |        | 2:03 |
| \\_      A very long leaf headline that... |        | 0:01 |
#+END:

後文: 日本語の文字を保持します。
")
 (:id "empty-default-table" :org-clocktable-body "| Headline     | Time   |
|--------------+--------|
| *Total time* | *0:00* |
" :org-updated-document "Before empty clocktable.

#+BEGIN: clocktable :maxlevel 0 :indent nil :narrow 1
| Headline     | Time   |
|--------------+--------|
| *Total time* | *0:00* |
#+END:

After empty clocktable.
")
 (:id "raw-pipe-title" :org-clocktable-body "| Headline     | Time   |      |
|--------------+--------+------|
| *Total time* | *0:05* |      |
|--------------+--------+------|
| Title        | pipe   | 0:05 |
" :org-updated-document "Before pipe-title clocktable.

#+BEGIN: clocktable :tags t
| Headline     | Time   |      |
|--------------+--------+------|
| *Total time* | *0:05* |      |
|--------------+--------+------|
| Title        | pipe   | 0:05 |
#+END:

After pipe-title clocktable.
")
 (:id "japanese-title-exceeds-display-narrow" :org-clocktable-body "| Headline                                               | Time   |
|--------------------------------------------------------+--------|
| *Total time*                                           | *0:05* |
|--------------------------------------------------------+--------|
| 日本語の表示幅を確認するための長いclocktable見出しです | 0:05   |
" :org-updated-document "日本語の見出し幅を検証します。

#+BEGIN: clocktable :narrow 40! :maxlevel 1
| Headline                                               | Time   |
|--------------------------------------------------------+--------|
| *Total time*                                           | *0:05* |
|--------------------------------------------------------+--------|
| 日本語の表示幅を確認するための長いclocktable見出しです | 0:05   |
#+END:

日本語の後続本文を保持します。
")))
