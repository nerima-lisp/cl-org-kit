;;; generate-clocktable-oracle.el --- Emit Org clocktable oracle fixtures -*- lexical-binding: t; -*-

;; Usage, from the repository root:
;;
;;   emacs --batch -Q --script scripts/generate-clocktable-oracle.el \
;;     t/fixtures/clocktable/oracle
;;
;; The directory holds `manifest.tsv' and `corpus/'.  For each manifest row the
;; script copies the corpus to a scratch directory, appends one clocktable
;; block to the host file, runs `org-update-dblock' in Emacs, and writes the
;; resulting host file to `expected/<case>.org'.  The scratch directory name is
;; replaced by `@DIR@' in the output.  A `provenance.meta' file records the
;; versions and input hashes.
;;
;; Manifest columns (tab separated, `#' lines are comments):
;;   case  host  agenda-files  zone  now  arguments
;; HOST and the comma-separated AGENDA-FILES name files in `corpus/'; `-' means
;; no agenda files.  ZONE is a TZ value, NOW is "YYYY-MM-DD HH:MM" in ZONE, and
;; ARGUMENTS is the text that follows `#+BEGIN: clocktable'.
;;
;; The clocktable caption and relative `:block' values read the wall clock, so
;; the script rebinds `current-time' and `format-time-string' to NOW.

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-clock)

(defconst clocktable-oracle--org-version "9.8.10")

(defun clocktable-oracle--read-file (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun clocktable-oracle--write-file (file content)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file (insert content))))

(defun clocktable-oracle--manifest (file)
  (let (rows)
    (dolist (line (split-string (clocktable-oracle--read-file file) "\n" t))
      (unless (or (string-prefix-p "#" line) (string-prefix-p "case\t" line))
        (let ((fields (split-string line "\t")))
          (unless (= (length fields) 6)
            (error "Manifest row needs 6 fields: %s" line))
          (push fields rows))))
    (nreverse rows)))

(defun clocktable-oracle--now (text zone)
  (set-time-zone-rule zone)
  (let ((parsed (parse-time-string (concat text ":00"))))
    (encode-time (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
                 (nth 3 parsed) (nth 4 parsed) (nth 5 parsed) zone)))

(defun clocktable-oracle--run-case (corpus row output)
  (pcase-let ((`(,case ,host ,files ,zone ,now ,arguments) row))
    (let* ((scratch (file-name-as-directory
                     (make-temp-file "clocktable-oracle" t)))
           (fixed-now (clocktable-oracle--now now zone))
           (agenda (unless (equal files "-")
                     (mapcar (lambda (name) (expand-file-name name scratch))
                             (split-string files ","))))
           (host-file (expand-file-name host scratch)))
      (unwind-protect
          (progn
            (dolist (source (directory-files corpus t "\\.org\\'"))
              (copy-file source (expand-file-name
                                 (file-name-nondirectory source) scratch)))
            (clocktable-oracle--write-file
             host-file
             (concat (clocktable-oracle--read-file host-file)
                     (format "#+BEGIN: clocktable %s\n#+END:\n" arguments)))
            (let ((org-agenda-files agenda)
                  (org-agenda-skip-unavailable-files nil)
                  (org-startup-folded nil)
                  (inhibit-message t)
                  (original-format-time-string
                   (symbol-function 'format-time-string)))
              (cl-letf (((symbol-function 'current-time)
                         (lambda () fixed-now))
                        ((symbol-function 'format-time-string)
                         (lambda (format &optional time &rest more)
                           (apply original-format-time-string
                                  format (or time fixed-now) more))))
                (with-current-buffer (find-file-noselect host-file)
                  (org-mode)
                  (goto-char (point-min))
                  (re-search-forward "^#\\+BEGIN: clocktable")
                  (beginning-of-line)
                  (org-update-dblock)
                  (save-buffer))))
            (clocktable-oracle--write-file
             (expand-file-name (concat case ".org") output)
             (string-replace
              (directory-file-name scratch) "@DIR@"
              (clocktable-oracle--read-file host-file))))
        (dolist (buffer (buffer-list))
          (when (and (buffer-file-name buffer)
                     (string-prefix-p scratch (buffer-file-name buffer)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (delete-directory scratch t)))))

(defun clocktable-oracle--sha256 (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun clocktable-oracle-main (directory)
  (unless (equal (org-version) clocktable-oracle--org-version)
    (error "Org %s is required, found %s"
           clocktable-oracle--org-version (org-version)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (corpus (concat directory "corpus/"))
         (manifest (concat directory "manifest.tsv"))
         (output (concat directory "expected/"))
         (rows (clocktable-oracle--manifest manifest)))
    (make-directory output t)
    (dolist (stale (directory-files output t "\\.org\\'"))
      (delete-file stale))
    (dolist (row rows)
      (clocktable-oracle--run-case corpus row output)
      (princ (format "Wrote %s\n" (car row))))
    (clocktable-oracle--write-file
     (concat directory "provenance.meta")
     (concat
      (format "emacs-major=%d\n" emacs-major-version)
      (format "org=%s\n" (org-version))
      "generator=scripts/generate-clocktable-oracle.el\n"
      (format "manifest-sha256=%s\n" (clocktable-oracle--sha256 manifest))
      (mapconcat
       (lambda (file)
         (format "corpus/%s-sha256=%s\n" (file-name-nondirectory file)
                 (clocktable-oracle--sha256 file)))
       (directory-files corpus t "\\.org\\'") "")))
    (princ (format "Generated %d clocktable oracle cases.\n" (length rows)))))

(clocktable-oracle-main (or (car command-line-args-left)
                            (error "Usage: generate-clocktable-oracle.el DIRECTORY")))
(setq command-line-args-left nil)
