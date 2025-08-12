;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'with-simulated-input)
(require 's)
(require 'keepachangelog)

(describe "keepachangelog-open"
  :var (test-root)

  (it "throws if no changelog is found"
    (assess-with-filesystem
        '(("lisp/source.el"))
      (setq test-root default-directory)
      (find-file "lisp/source.el")

      (expect (keepachangelog-open) :to-throw)))

  (it "switches to the changlog buffer"
    (with-simulated-input "Added RET"
      (assess-with-filesystem
          '(("CHANGELOG.md")
            ("lisp/source.el"))
        (setq test-root default-directory)
        (find-file "lisp/source.el")

        (keepachangelog-open)

        (expect (s-chop-prefix test-root (buffer-file-name (window-buffer))) :to-equal "/CHANGELOG.md"))))

  (it "moves to the first version header"
    (with-simulated-input "Added RET"
      (assess-with-filesystem
          '(("CHANGELOG.md" "Preface
## Version
")
            ("lisp/source.el"))
        (find-file "lisp/source.el")

        (keepachangelog-open)
        (expect (looking-at "## Version") :to-be-truthy)))))

(describe "keepachangelog-add-entry"
  :var (test-root)

  (it "opens changelog and adds entry"
    (with-simulated-input "Added RET"
      (assess-with-filesystem
          '(("CHANGELOG.md" "Preface
## Version
")
            ("lisp/source.el"))
        (find-file "lisp/source.el")

        (keepachangelog-add-entry)
        (expect (buffer-string) :to-equal "Preface\n## Version\n\n### Added\n- \n")
        (beginning-of-line)
        (expect (looking-at "- ") :to-be-truthy)))))
