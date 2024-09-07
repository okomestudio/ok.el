;;; test-ok-file.el --- Tests for ok-file.el  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Code:

(require 'buttercup)
(require 'ok-file)

(describe "ok-file-ensure-from-url"
  :var (src dest dest-abs dest-generated dest-pre-existing)
  (before-each
    (setq src (make-temp-name "ok-file-"))
    (setq dest (make-temp-name "ok-file-"))
    (setq dest-abs (expand-file-name dest))
    (setq dest-generated (expand-file-name (file-name-nondirectory src)))
    (setq dest-pre-existing "/dev/null")
    (spy-on 'url-copy-file))

  (it "skips if `dest' already exists"
    (expect (ok-file-ensure-from-url src dest-pre-existing) :to-equal nil)
    (expect (spy-calls-count 'url-copy-file) :to-equal 0))

  (it "expands the relative `dest'"
    (expect (ok-file-ensure-from-url src dest) :to-equal dest-abs)
    (expect 'url-copy-file :to-have-been-called-with src dest-abs))

  (it "passes the absolute `dest' as is"
    (expect (ok-file-ensure-from-url src dest-abs) :to-equal dest-abs)
    (expect 'url-copy-file :to-have-been-called-with src dest-abs))

  (it "uses `src' filename as `dest' when not supplied"
    (expect (ok-file-ensure-from-url src) :to-equal dest-generated)
    (expect 'url-copy-file :to-have-been-called-with src dest-generated)))
