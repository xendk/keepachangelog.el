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

(provide 'helpers)
