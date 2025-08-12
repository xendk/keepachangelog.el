;;; keepachangelog.el --- Keep a keepachangelog.com format CHANGELOG.md file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "29"))
;; Package-Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar keepachangelog--sections '("Added"
                                   "Changed"
                                   "Deprecated"
                                   "Removed"
                                   "Fixed"
                                   "Security")
  "Known section types and their order.")

(defvar keepachangelog--section-history nil "Section history.")

;; We don't need a big history, it's mostly the last few items that
;; might be reused.
(put 'keepachangelog--section-history 'history-length 10)

(defmacro keepachangelog-with-current-version (&rest body)
  "Narrow to current version and run BODY.

The block is run with point at the beginning. The narrow does not
include the empty line between this and the following versions but it
ensures that it exists.

Raises user-error if not inside a version."
  (declare (indent 0) (debug t))
  `(let ((beg (keepachangelog--find-version -1 t))
         (end (or (keepachangelog--find-version) (point-max))))
     (unless beg
       (user-error "Not inside a version"))
     ;; Deal with following newlines.
     (unless (equal end (point-max))
       (save-excursion
         (goto-char end)
         ;; Back up a line when on a version header.
         (forward-line -1)
         ;; Back up end while there's empty lines.
         (when (looking-at "^$")
           (setq end (point)))
         (while (looking-at "^$")
           (forward-line -1)
           (when (looking-at "^$")
             (setq end (point))))
         ;; Ensure the line between versions.
         (forward-line)
         (unless (looking-at "^$") (insert "\n"))))
     (with-restriction beg end
       (goto-char (point-min))
       (progn ,@body))))

;;;###autoload
(defun keepachangelog-open ()
  "Open the nearest CHANGELOG.md."
  (interactive)
  (let ((change-log-dir (locate-dominating-file default-directory "CHANGELOG.md")))
    (if change-log-dir
        (progn
          (find-file (concat change-log-dir "CHANGELOG.md"))
          (goto-char (point-min))
          (when-let* ((point (keepachangelog--find-version)))
            (keepachangelog-next-version)))
      (user-error "Could not find CHANGELOG.md"))))

;;;###autoload
(defun keepachangelog-add-entry ()
  "Open CHANGELOG.md and add new entry."
  (interactive)
  (keepachangelog-open)
  (call-interactively 'keepachangelog-add-entry-to-section))

(defun keepachangelog-next-version ()
  "Move to the next version header."
  (interactive)
  (let ((point (keepachangelog--find-version)))
    (if point (goto-char point)
      (user-error "No more version headers"))))

(defun keepachangelog-add-entry-to-section (section)
  "Add an empty entry to SECTION of version at point.

Will find the SECTION to add an item to or create it if
necessary (ensuring the proper section order).

When called interactively, prompt for section to add to."
  (interactive (list
                (completing-read "Add entry to section: "
                                 keepachangelog--sections
                                 nil
                                 'confirm
                                 nil
                                 'keepachangelog--section-history)))
  (keepachangelog-with-current-version
    ;; Skip to either sections or end.
    (goto-char (or (keepachangelog--find-line (concat "### ") 1 t)
                   (point-max)))
    (keepachangelog--section-find-or-insert section)
    (keepachangelog--section-add-entry)))

(defun keepachangelog--find-version (&optional count allow-current)
  "Find the next/previous release.

COUNT defines direction and number to skip, ALLOW-CURRENT defines wether
to the current line is considered."
  (keepachangelog--find-line "## " count allow-current))

(defun keepachangelog--find-line (regex &optional count allow-current)
  "Find the next/previous line matching REGEX at start of line.

COUNT defines direction and number to skip, ALLOW-CURRENT defines wether
the current line is considered."
  (let ((count (or count 1)))
    (save-excursion
      ;; Skip forward if we're already on a match header.
      (beginning-of-line)
      (when (looking-at regex)
        ;; Skip to end/start of line when searching forward/backwards
        ;; to avoid matching current line, start/end when
        ;; allow-current is t.
        (if (xor allow-current (cl-plusp count))
            (end-of-line)
          (beginning-of-line)))
      (condition-case nil
          (progn (re-search-forward (concat "^" regex) nil nil count)
                 (beginning-of-line)
                 (point))
        (error nil)))))

;;; Section functions.

(defun keepachangelog--section-find-or-insert (section)
  "Find or add a SECTION section after point.

Expects to be on the first line of sections (which may be empty)."
  (if-let* ((pos (keepachangelog--find-line (concat "### " section) 1 t)))
      (goto-char pos)
    ;; Loop over sections until the one we're inserting.
    (let* ((sections keepachangelog--sections)
           found)
      (while (and sections (not (equal (car sections) section)))
        (when-let* ((pos (keepachangelog--find-line (concat "### " (car sections)) 1 t)))
          (goto-char pos)
          (setq found t))
        (setq sections (cdr sections)))
      (keepachangelog--section-insert section found))))

(defun keepachangelog--section-skip-to-end ()
  "Skip to end of current section.

That is, the following empty line."
  (beginning-of-line)
  ;; We'll allow an empty line after the header, which complicates
  ;; things a bit.
  (when (looking-at "###")
    (forward-line)
    (when (and
           (looking-at "^$")
           ;; Peek the next line.
           (save-excursion
             (forward-line)
             (looking-at "^-")))
      (forward-line)))
  (while (and (looking-at "^[- \\t].*$") (not (eobp)))
    (forward-line))
  ;; Ensure there's at least one empty line if we're at buffer end.
  (when (and (not (looking-at "^$")) (eobp))
    (insert "\n")))

(defun keepachangelog--section-insert (section &optional after)
  "Insert SECTION at point, ensuring the proper surrounding whitespace.

If there's a section header at point, insert the new one before unless
AFTER is t."
  (beginning-of-line)
  (when (looking-at "^### ")
    (if after
        (progn
          (keepachangelog--section-skip-to-end)
          (insert "\n"))
      (save-excursion (insert "\n"))))

  ;; Make sure there's an empty line before.
  (save-excursion
    (forward-line -1)
    (unless (looking-at "^[[:blank:]]*$") (end-of-line)(insert "\n")))
   (save-excursion
     (insert "### " section "\n")
     (unless (looking-at "^[[:blank:]]*$") (insert "\n"))))

(defun keepachangelog--section-add-entry ()
  "Add an empty entry to the current section."
  (keepachangelog--section-skip-to-end)
  (insert "- \n")
  (backward-char))

(provide 'keepachangelog)
;;; keepachangelog.el ends here
