;;; test-ok-datetime.el --- Tests for ok-datetime.el  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Code:

(require 'buttercup)
(require 'ok-datetime)

(describe "ok-datetime-parse-human-readable"
  :var (result)
  (it "parses a human readable date"
    (setq result (ok-datetime-parse-human-readable "June 23, 2014"))
    (expect (listp result) :to-equal t)
    (expect (nth 3 result) :to-equal 23)
    (expect (nth 4 result) :to-equal 6)
    (expect (nth 5 result) :to-equal 2014))

  (it "parses a bad input to a list mostly with nil values"
    (setq result (ok-datetime-parse-human-readable "bad input"))
    (expect (listp result) :to-equal t)
    (dolist (i '(0 1 2 3 4 5 6 8))
      (expect (nth i result) :to-equal nil))
    (expect (nth 7 result) :to-equal -1)))
