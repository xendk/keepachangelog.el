;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 's)
(require 'keepachangelog)

(describe "keepachangelog-add-entry"
  :var (test-root)

  (it "throws if no changelog is found"
    (assess-with-filesystem
     '(("lisp/source.el"))
     (setq test-root default-directory)
     (find-file "lisp/source.el")

     (expect (keepachangelog-add-entry) :to-throw)))

  (it "switches to the changlog buffer"
    (assess-with-filesystem
     '(("CHANGELOG.md")
       ("lisp/source.el"))
     (setq test-root default-directory)
     (find-file "lisp/source.el")

     (keepachangelog-add-entry)

     (expect (s-chop-prefix test-root (buffer-file-name (window-buffer))) :to-equal "/CHANGELOG.md"))))
