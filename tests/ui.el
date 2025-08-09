;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'keepachangelog)
(require 'helpers)

(describe "keepachangelog-add-entry-to-section"
  (it "adds entry to existing section"
    (with-buffer "## V1

### Add|ed
- one

### Deprecated
- none
"
      (keepachangelog-add-entry-to-section "Added")

      (expect (buffer-string) :to-equal "## V1\n\n### Added\n- one\n- \n\n### Deprecated\n- none\n")
      (forward-line -1)
      (expect (looking-at "- one") :to-be-truthy)))

  (it "inserts into the current version regardless of the following"
    (with-buffer "## |V1

### Removed

## V2

### Added"
      (keepachangelog-add-entry-to-section "Added")
      (expect (buffer-string) :to-equal "## V1\n\n### Added\n- \n\n### Removed\n\n## V2\n\n### Added")
      )))
