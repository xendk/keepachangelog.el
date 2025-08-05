;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'assess)
(require 'helpers)

(describe "test helpers"
  (it "with-buffer moves the point to the point of the first |"
    (with-buffer "lets|test"
      (expect (buffer-substring (point) (point-max)) :to-equal "test"))))

(provide 'helpers)
