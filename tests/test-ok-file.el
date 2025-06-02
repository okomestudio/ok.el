;;; test-ok-file.el --- Tests for ok-file.el  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Code:

(require 'buttercup)
(require 'ok-file)

(describe
 "ok-file-ensure-from-url"
 :var (src dest dest-abs dest-generated dest-pre-existing)
 (before-each
  (setq src (make-temp-name "ok-file-"))
  (setq dest (make-temp-name "ok-file-"))
  (setq dest-abs (expand-file-name dest))
  (setq dest-generated (expand-file-name (file-name-nondirectory src)))
  (setq dest-pre-existing "/dev/null")
  (setq dest-dir-existing (make-temp-file "ok-file-" t))
  (setq dest-dir-existing-generated
        (expand-file-name (file-name-nondirectory src)
                          dest-dir-existing))
  (spy-on 'url-copy-file))

 (after-each
  (delete-directory dest-dir-existing))

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
     (expect 'url-copy-file :to-have-been-called-with src dest-generated))

 (it "adds `src' filename to `dest' pointing to an existing directory"
     (expect (ok-file-ensure-from-url src dest-dir-existing)
             :to-equal dest-dir-existing-generated)
     (expect 'url-copy-file
             :to-have-been-called-with src dest-dir-existing-generated)))

(describe
 "ok-file-parent-directory"
 (it "removes file component"
     (expect (ok-file-parent-directory "/a/b/c.txt") :to-equal "/a/b/"))
 (it "removes directory component"
     (expect (ok-file-parent-directory "/a/b/c/") :to-equal "/a/b/"))
 (it "returns nil when at root"
     (expect (ok-file-parent-directory "/") :to-equal nil))
 (it "returns nil when empty"
     (expect (ok-file-parent-directory "") :to-equal nil))
 (it "returns nil when empty"
     (expect (ok-file-parent-directory "  ") :to-equal nil)))
