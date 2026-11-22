;;; generate-planning-oracle.el --- Emit Org planning-line edit fixtures -*- lexical-binding: t; -*-

;; Usage, from the repository root:
;;
;;   emacs --batch -Q --script scripts/generate-planning-oracle.el \
;;     t/fixtures/e/oracle/planning/cases.sexp
;;
;; Each case runs one Org command on the heading titled "one" and records the
;; resulting buffer text.  The kit tests replay the same operation and compare.
;; Org 9.8.10 is required; the recorded Emacs version is whatever ran.

(require 'org)
(require 'cl-lib)
(require 'pp)

(defconst planning-oracle--fixed-time
  (encode-time (list 0 20 14 22 11 2026 nil nil t))
  "The instant that stands in for the current time: 2026-11-22 14:20 UTC.")

(defconst planning-oracle--todo-header "#+TODO: TODO | DONE\n#+STARTUP: logdone\n")

(defconst planning-oracle--cases
  `(;; Completing a heading writes CLOSED in front of the existing keywords.
    (:name "todo-done-scheduled"
     :source ,(concat planning-oracle--todo-header
                      "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n")
     :op (:todo "DONE"))
    (:name "todo-done-deadline-and-scheduled"
     :source ,(concat planning-oracle--todo-header
                      "* Cat\n** TODO one\n"
                      "DEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>\n")
     :op (:todo "DONE"))
    (:name "todo-done-bare"
     :source ,(concat planning-oracle--todo-header "* Cat\n** TODO one\n")
     :op (:todo "DONE"))
    (:name "todo-done-property-drawer"
     :source ,(concat planning-oracle--todo-header
                      "* Cat\n** TODO one\n:PROPERTIES:\n:A: b\n:END:\n")
     :op (:todo "DONE"))
    (:name "todo-done-empty-heading-blank-after"
     :source ,(concat planning-oracle--todo-header
                      "* Cat\n** TODO one\n\n** Next\n")
     :op (:todo "DONE"))
    (:name "todo-done-scheduled-and-property-drawer"
     :source ,(concat planning-oracle--todo-header
                      "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n"
                      ":PROPERTIES:\n:A: b\n:END:\n")
     :op (:todo "DONE"))
    ;; Setting a keyword puts it in front; a re-set keyword moves to the front.
    (:name "set-scheduled-new"
     :source "* Cat\n** TODO one\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-deadline-new"
     :source "* Cat\n** TODO one\n"
     :op (:set :deadline "2026-11-30"))
    (:name "set-scheduled-empty-heading-blank-after"
     :source "* Cat\n** TODO one\n\n** Next\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-scheduled-before-deadline"
     :source "* Cat\n** TODO one\nDEADLINE: <2026-11-25 Wed>\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-deadline-before-scheduled"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n"
     :op (:set :deadline "2026-11-30"))
    (:name "set-scheduled-existing-first"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat> DEADLINE: <2026-11-25 Wed>\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-scheduled-existing-last"
     :source "* Cat\n** TODO one\nDEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-deadline-indented-planning"
     :source "* Cat\n** TODO one\n  SCHEDULED: <2026-11-21 Sat>\n"
     :op (:set :deadline "2026-11-30"))
    (:name "set-closed-indented-planning"
     :source "* Cat\n** DONE one\n  SCHEDULED: <2026-11-21 Sat>\n"
     :op (:set :closed "2026-11-20 10:00"))
    (:name "set-scheduled-before-property-drawer"
     :source "* Cat\n** TODO one\n:PROPERTIES:\n:A: b\n:END:\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-closed-before-scheduled"
     :source "* Cat\n** DONE one\nSCHEDULED: <2026-11-21 Sat>\n"
     :op (:set :closed "2026-11-20 10:00"))
    (:name "set-closed-before-property-drawer"
     :source "* Cat\n** DONE one\n:PROPERTIES:\n:A: b\n:END:\n"
     :op (:set :closed "2026-11-20 10:00"))
    ;; Removing the last keyword deletes the planning line and nothing after it.
    (:name "remove-only-scheduled-blank-after"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n\nbody\n"
     :op (:remove :scheduled))
    (:name "remove-only-deadline-blank-after"
     :source "* Cat\n** TODO one\nDEADLINE: <2026-11-25 Wed>\n\nbody\n"
     :op (:remove :deadline))
    (:name "remove-only-closed-blank-after"
     :source "* Cat\n** DONE one\nCLOSED: [2026-11-20 Fri 10:00]\n\nbody\n"
     :op (:remove :closed))
    (:name "remove-only-no-blank-after"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\nbody\n"
     :op (:remove :scheduled))
    (:name "remove-only-before-next-heading"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n\n** Next\n"
     :op (:remove :scheduled))
    (:name "remove-only-no-blank-before-next-heading"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n** Next\n"
     :op (:remove :scheduled))
    (:name "remove-only-blank-before-child-heading"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n\n*** Child\n"
     :op (:remove :scheduled))
    (:name "remove-only-blank-line-at-end-without-newline"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n  "
     :op (:remove :scheduled))
    (:name "remove-only-before-property-drawer"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat>\n:PROPERTIES:\n:A: b\n:END:\nbody\n"
     :op (:remove :scheduled))
    (:name "remove-scheduled-keep-deadline"
     :source "* Cat\n** TODO one\nDEADLINE: <2026-11-25 Wed> SCHEDULED: <2026-11-21 Sat>\n\nbody\n"
     :op (:remove :scheduled))
    (:name "remove-deadline-keep-closed"
     :source "* Cat\n** DONE one\nCLOSED: [2026-11-20 Fri 10:00] DEADLINE: <2026-11-25 Wed>\n\nbody\n"
     :op (:remove :deadline))
    (:name "remove-deadline-keep-closed-before-property-drawer"
     :source "* Cat\n** DONE one\nCLOSED: [2026-11-20 Fri 10:00] DEADLINE: <2026-11-25 Wed>\n:PROPERTIES:\n:A: b\n:END:\n"
     :op (:remove :deadline))
    (:name "remove-closed-at-end-of-line"
     :source "* Cat\n** DONE one\nSCHEDULED: <2026-11-21 Sat> CLOSED: [2026-11-20 Fri 10:00]\n\nbody\n"
     :op (:remove :closed))
    (:name "set-closed-again-at-end-of-line"
     :source "* Cat\n** DONE one\nSCHEDULED: <2026-11-21 Sat> CLOSED: [2026-11-20 Fri 10:00]\n"
     :op (:set :closed "2026-11-22 09:00"))
    ;; A line that starts with a planning keyword is a planning line even when
    ;; no valid timestamp follows.
    (:name "set-scheduled-on-closed-junk"
     :source "* Cat\n** TODO one\nCLOSED: junk\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-scheduled-on-scheduled-junk"
     :source "* Cat\n** TODO one\nSCHEDULED: junk\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-scheduled-on-keyword-only"
     :source "* Cat\n** TODO one\nSCHEDULED:\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :scheduled "2026-11-30"))
    (:name "set-closed-on-indented-closed-junk"
     :source "* Cat\n** DONE one\n  CLOSED: junk\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :closed "2026-11-20 10:00"))
    (:name "set-deadline-on-junk-deadline-then-scheduled"
     :source "* Cat\n** TODO one\nDEADLINE: junk SCHEDULED: <2026-11-21 Sat>\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :deadline "2026-11-30"))
    (:name "set-closed-on-scheduled-then-deadline-junk"
     :source "* Cat\n** DONE one\nSCHEDULED: <2026-11-21 Sat> DEADLINE: junk\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :closed "2026-11-20 10:00"))
    (:name "remove-scheduled-keep-deadline-junk"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat> DEADLINE: junk\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:remove :scheduled))
    (:name "remove-scheduled-keep-trailing-text"
     :source "* Cat\n** TODO one\nSCHEDULED: <2026-11-21 Sat> trailing text\n\nbody\n"
     :op (:remove :scheduled))
    (:name "set-closed-on-scheduled-with-trailing-text"
     :source "* Cat\n** DONE one\nSCHEDULED: <2026-11-21 Sat> trailing text\n:PROPERTIES:\n:ID: x\n:END:\n"
     :op (:set :closed "2026-11-20 10:00"))
    (:name "remove-closed-keep-scheduled"
     :source "* Cat\n** TODO one\nCLOSED: [2026-11-20 Fri 10:00] SCHEDULED: <2026-11-21 Sat>\n\nbody\n"
     :op (:remove :closed))))

(defun planning-oracle--goto-heading ()
  (goto-char (point-min))
  (unless (re-search-forward "^\\*+ \\(?:[A-Z]+ \\)?one$" nil t)
    (error "Planning oracle case has no heading titled one"))
  (beginning-of-line))

(defun planning-oracle--apply (op)
  (pcase op
    (`(:todo ,state) (org-todo state))
    (`(:set :scheduled ,date) (org-schedule nil date))
    (`(:set :deadline ,date) (org-deadline nil date))
    (`(:set :closed ,date) (org-add-planning-info 'closed date))
    (`(:remove :scheduled) (org-schedule '(4)))
    (`(:remove :deadline) (org-deadline '(4)))
    (`(:remove :closed) (org-add-planning-info nil nil 'closed))
    (_ (error "Unknown planning oracle operation %S" op))))

(defun planning-oracle--timestamp (op)
  "Return the timestamp text Org wrote for a :set OP, or nil for other ops.
The new field goes first on the planning line, so the first match is it."
  (pcase op
    (`(:set ,kind ,_)
     (save-excursion
       (forward-line)
       (when (re-search-forward
              (concat (upcase (substring (symbol-name kind) 1))
                      ": \\([[<][^]>]+[]>]\\)")
              (line-end-position) t)
         (match-string-no-properties 1))))))

(defun planning-oracle--run (case)
  (let ((op (plist-get case :op))
        (org-inhibit-startup t)
        (org-adapt-indentation nil)
        (inhibit-message t))
    (with-temp-buffer
      (insert (plist-get case :source))
      (org-mode)
      (org-set-regexps-and-options)
      (planning-oracle--goto-heading)
      (cl-letf (((symbol-function 'current-time)
                 (lambda () planning-oracle--fixed-time)))
        (planning-oracle--apply op))
      (planning-oracle--goto-heading)
      (list :name (plist-get case :name)
            :emacs-version emacs-version
            :org-version (org-version)
            :source-input (plist-get case :source)
            :operation op
            :timestamp (planning-oracle--timestamp op)
            :expected (buffer-substring-no-properties (point-min) (point-max))))))

(defun planning-oracle--generate (output)
  (unless (equal (org-version) "9.8.10")
    (error "Planning oracle requires Org 9.8.10; found %s" (org-version)))
  (setenv "TZ" "UTC")
  (let ((results (mapcar #'planning-oracle--run planning-oracle--cases))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory (expand-file-name output)) t)
    (with-temp-file output
      (insert (format "; Generated from Org planning commands with Emacs %s and Org %s.\n"
                      emacs-version (org-version))
              "; Each case replays one command on the heading titled \"one\".\n")
      (let ((print-escape-newlines nil)
            (pp-escape-newlines nil)
            (indent-tabs-mode nil))
        (pp (list :name "planning-oracle"
                  :emacs-version emacs-version
                  :org-version (org-version)
                  :cases results)
            (current-buffer))))
    (princ (format "Wrote %d planning oracle cases to %s\n" (length results) output))))

(let ((arguments (if (equal (car command-line-args-left) "--")
                     (cdr command-line-args-left)
                   command-line-args-left)))
  (setq command-line-args-left nil)
  (unless (= (length arguments) 1)
    (error "Usage: emacs --batch -Q --script scripts/generate-planning-oracle.el OUTPUT"))
  (planning-oracle--generate (car arguments)))
