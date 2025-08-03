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

;;;###autoload
(defun keepachangelog-add-entry ()
  "Open CHANGELOG.md for adding a new entry."
  (interactive)
  (let ((change-log-dir (locate-dominating-file (buffer-file-name) "CHANGELOG.md")))
    (if change-log-dir
        (find-file (concat change-log-dir "CHANGELOG.md"))
      (user-error "Could not find CHANGELOG.md"))))

(defun keepachangelog-next-version ()
  "Move to the next version header."
  (interactive)
  (let ((point (keepachangelog--find-next-version)))
    (if point (goto-char point)
      (user-error "No more version headers"))))

(defun keepachangelog--find-next-version ()
  "Return start of next version."
  (save-excursion
    ;; Skip forward if we're already on a version header.
    (when (looking-at (rx bol "## "))
      (forward-line))
    (condition-case nil
        (progn (re-search-forward (rx bol "## "))
               (beginning-of-line)
               (point))
      (error nil))))

(provide 'keepachangelog)
;;; keepachangelog.el ends here
