;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'keepachangelog)
(require 'helpers)

(describe "keepachangelog--section-add-entry"
  (it "adds a new entry on just header"
    (with-buffer "### Added|"

      (keepachangelog--section-add-entry)

      (expect (buffer-string) :to-equal "### Added\n- \n")
      (expect (equal (point) (- (point-max) 1)) :to-be-truthy))))
