#!/usr/local/bin/mzscheme
#lang scheme/base
(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3:4/text-ui))
(require scheme/path)
(require (lib "etc.ss"))

(require "../mysql.ss")

(define connection-tests
  (test-suite
   "Tests for mr-mysql connection"
   ;; connect

   (let
       ((connection (mysql-connect #:hostname "localhost" #:username "root" #:database "test")))
     (check-not-false connection "Can connect")
     (mysql-disconnect connection))

   (check-exn exn?
              (lambda ()
                (mysql-connect #:hostname "http://example.org" #:username "root" #:database "test"))
              "Raise error on wrong host")

   (check-exn exn?
              (lambda ()
                (mysql-connect #:hostname "localhost" #:username "root" #:database "no-such-database"))
              "Raise error on wrong database")
   ))

;; autorun
(exit (when (equal?
             (normalize-path (find-system-path 'run-file))
             (normalize-path (this-expression-file-name)))
        (run-tests connection-tests)))
