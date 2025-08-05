;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'keepachangelog)
(require 'helpers)

(describe "keepachangelog--find-or-insert-section"
  (it "adds a new section"
    (with-buffer "|"

      (keepachangelog--find-or-insert-section "Added")

      (expect (buffer-string) :to-equal "### Added\n")))

  (it "jumps to existing section "
    (with-buffer "|Preface
### Added
- stuff"

      (keepachangelog--find-or-insert-section "Added")

      (expect (looking-at "### Added") :to-be-truthy)
      (expect (buffer-string) :to-equal "Preface
### Added
- stuff")))

  (it "inserts sections in the appropriate order"
    (with-buffer "|Preface
### Added
### Deprecated"

      (keepachangelog--find-or-insert-section "Changed")

      (expect (buffer-string) :to-equal "Preface
### Added

### Changed

### Deprecated")
      (expect (looking-at "### Changed") :to-be-truthy)
      )))

(describe "keepachangelog--insert-section"
  (it "inserts section and surrounding empty lines"
    (with-buffer "### One
|### Three"

      (keepachangelog--insert-section "Two")

      (expect (buffer-string) :to-equal "### One

### Two

### Three")
      (expect (looking-at "### Two") :to-be-truthy)
      ))

  (it "does not add newlines at start and end of buffer"
    (with-buffer "|"

      (keepachangelog--insert-section "Two")

      (expect (buffer-string) :to-equal "### Two\n")
      (expect (looking-at "### Two") :to-be-truthy)
      ))

  (it "keeps superfluous surrounding newlines"
    (with-buffer "### One\n\n\n|\n\n\n### Three"

      (keepachangelog--insert-section "Two")

      (expect (buffer-string) :to-equal "### One\n\n\n### Two\n\n\n\n### Three")
      (expect (looking-at "### Two") :to-be-truthy)
      )))
