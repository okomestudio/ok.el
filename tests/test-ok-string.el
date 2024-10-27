;;; test-ok-string.el --- Tests for ok-string.el  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Code:

(require 'buttercup)
(require 'ok-string)

(describe "ok-string-format"
  (it "expands named fields"
    (expect (ok-string-format "foo %(bar)s baz" '((bar . "BAR")))
            :to-equal "foo BAR baz")))
