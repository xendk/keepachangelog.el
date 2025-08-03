;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'keepachangelog)

(describe "keepachangelog-find-next-version"
  (it "moves point to next version header"
    (assess-as-temp-buffer "Preface
## First version
## Second version"
                           (goto-char (point-min))

                           (keepachangelog-find-next-version)
                           (expect (looking-at "## First version") :to-be-truthy)
                           (keepachangelog-find-next-version)
                           (expect (looking-at "## Second version") :to-be-truthy)))

  (it "user-errors if no more version headers found"
    (assess-as-temp-buffer "Preface
## First version
No more"
                           (goto-char (point-min))

                           (keepachangelog-find-next-version)
                           (expect (looking-at "## First version") :to-be-truthy)

                           (expect (keepachangelog-find-next-version) :to-throw 'user-error '("No more version headers"))))
  (it "doesn't move point if no more version headers found"
    (assess-as-temp-buffer "Preface
## First version
No more"
                           (goto-char (point-min))

                           (keepachangelog-find-next-version)
                           (expect (keepachangelog-find-next-version) :to-throw 'user-error '("No more version headers"))
                           (expect (looking-at "## First version") :to-be-truthy)
                           )))
