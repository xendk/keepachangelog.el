;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'keepachangelog)

(defun line-at-point (point)
  "Helper to get the line at POINT."
  (save-excursion
    (let (start end)
      (goto-char point)
      (beginning-of-line)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (buffer-substring start end))))

(defmacro with-buffer (x &rest body)
  (declare (indent 1) (debug t))
  `(let ((parts (split-string ,x "|")))
     (assess-as-temp-buffer (apply 'concat parts)
       ;; First cursor position in a buffer is 1, and an empty string
       ;; before the | string is the same.
       (goto-char (1+ (length (car parts))))
       ,@body)))

(provide 'helpers)
