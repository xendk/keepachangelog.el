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

(describe "keepachangelog-next-version"
  (it "moves point to next version header"
    (assess-as-temp-buffer "Preface
## First version
## Second version"
      (goto-char (point-min))

      (keepachangelog-next-version)
      (expect (looking-at "## First version") :to-be-truthy)
      (keepachangelog-next-version)
      (expect (looking-at "## Second version") :to-be-truthy)))

  (it "user-errors if no more version headers found"
    (assess-as-temp-buffer "Preface
## First version
No more"
      (goto-char (point-min))

      (keepachangelog-next-version)
      (expect (looking-at "## First version") :to-be-truthy)

      (expect (keepachangelog-next-version) :to-throw 'user-error '("No more version headers"))))
  (it "doesn't move point if no more version headers found"
    (assess-as-temp-buffer "Preface
## First version
No more"
      (goto-char (point-min))

      (keepachangelog-next-version)
      (expect (keepachangelog-next-version) :to-throw 'user-error '("No more version headers"))
      (expect (looking-at "## first version") :to-be-truthy))))

(describe "keepachangelog--find-version"
  :var ((simple-fixture "Preface
## First version
one
## Second version
second
## Third version
No more"))
  (it "finds the next version"
    (assess-as-temp-buffer simple-fixture
      (goto-char (point-min))

      (let ((point))
        (setq point (keepachangelog--find-version))
        (expect (line-at-point point) :to-equal "## First version")
        (goto-char point)
        (setq point (keepachangelog--find-version))
        (expect (line-at-point point) :to-equal "## Second version"))))

  (it "returns nil if no more versions found"
    (assess-as-temp-buffer simple-fixture
      (goto-char (point-min))
      (search-forward "No")
      (expect (keepachangelog--find-version) :not :to-be-truthy)))

  (it "finds the previous version"
    (assess-as-temp-buffer simple-fixture
      (goto-char (point-max))

      (let ((point))
        (setq point (keepachangelog--find-version -1))
        (expect (line-at-point point) :to-equal "## Third version")
        (goto-char point)
        (setq point (keepachangelog--find-version -1))
        (expect (line-at-point point) :to-equal "## Second version"))))

  (it "returns nil if no more previous versions found"
    (assess-as-temp-buffer simple-fixture
      (goto-char (point-min))
      (search-forward "face")
      (expect (keepachangelog--find-version -1) :not :to-be-truthy))))
