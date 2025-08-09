;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'keepachangelog)
(require 'helpers)

(describe "keepachangelog-with-current-version"
  (it "narrows to current version"
    (with-buffer "## V1

### Sec1
## V2

### |Sec2

## V3

### Sec3
"
      (let ((buffer-content nil))
        (keepachangelog-with-current-version
          (setq buffer-content (buffer-string)))
        (expect buffer-content :to-equal "## V2\n\n### Sec2\n\n"))))

  (it "user-errors if no previous version header"
    (with-buffer "Random preface

### |Not a version

## V3

### Sec3
"
      (expect (keepachangelog-with-current-version) :to-throw 'user-error '("Not inside a version")))))
