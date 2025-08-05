;;; keepachangelog.el --- Keep a keepachangelog.com format CHANGELOG.md file  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thomas Fini Hansen

;; Author: Thomas Fini Hansen <xen@xen.dk>
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "28"))
;; Package-Version: 0.0.1

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

;;;###autoload
(defun keepachangelog-add-entry ()
  "Open CHANGELOG.md for adding a new entry."
  (interactive)
  (let ((change-log-dir (locate-dominating-file default-directory "CHANGELOG.md")))
    (if change-log-dir
        (progn
          (find-file (concat change-log-dir "CHANGELOG.md"))
          (goto-char (point-min))
          (when-let ((point (keepachangelog--find-version)))
            (keepachangelog-next-version)))
      (user-error "Could not find CHANGELOG.md"))))

(defun keepachangelog-next-version ()
  "Move to the next version header."
  (interactive)
  (let ((point (keepachangelog--find-version)))
    (if point (goto-char point)
      (user-error "No more version headers"))))

(defun keepachangelog--find-or-insert-section (section)
  "Find or add a SECTION section after point."
  (if-let ((pos (keepachangelog--find (concat "### " section))))
      (goto-char pos)
    ;; Loop over sections until the one we're inserting.
    (let ((sections keepachangelog--sections))
      (while (and sections (not (equal (car sections) section)))
        (when-let ((pos (keepachangelog--find (concat "### " (car sections)))))
          (goto-char pos)
          (forward-line))
        (setq sections (cdr sections))))
    (keepachangelog--insert-section section)))

(defun keepachangelog--insert-section (section)
  "Insert SECTION at point, ensuring the proper surrounding whitespace."
  (unless (looking-at "^[[:blank:]]*$") (insert "\n"))
  (save-excursion
    (insert "### " section "\n")
    (unless (looking-at "^[[:blank:]]*$") (insert "\n"))))

(defun keepachangelog--find-version (&optional count)
  "Find the next/previous release.

COUNT defines direction and number to skip."
  (keepachangelog--find "## " count))

(defun keepachangelog--find (regex &optional count)
  "Find the next/previous line matching REGEX at start of line..

COUNT defines direction and number to skip."
  (let ((count (or count 1)))
    (save-excursion
      ;; Skip forward if we're already on a version header.
      (when (looking-at regex)
        (forward-line count))
      (condition-case nil
          (progn (re-search-forward regex nil nil count)
                 (beginning-of-line)
                 (point))
        (error nil)))))

(provide 'keepachangelog)
;;; keepachangelog.el ends here
