#!/usr/local/bin/mzscheme
#lang scheme/base
(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3:4/text-ui))
(require scheme/path)
(require (lib "etc.ss"))

(require "../mysql.ss")

(define mr-mysql-tests
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
   (let
       ((connection (mysql-connect #:hostname "localhost" #:username "root" #:database "test")))
     (check-equal? (mysql-escape-string "foobar" connection) "foobar" "Normal strings pass unaffected")
     (mysql-disconnect connection))
   (let
       ((connection (mysql-connect #:hostname "localhost" #:username "root" #:database "test")))
     (check-equal? (mysql-escape-string "foo'bar" connection) "foo\\'bar" "Single quotes gets escaped")
     (mysql-disconnect connection))
   (let
       ((connection (mysql-connect #:hostname "localhost" #:username "root" #:database "test")))
     (check-equal? (mysql-escape-string "blåbærgrød" connection) "blåbærgrød" "high-bytes passes through unaffected")
     (mysql-disconnect connection))
   ))

;; autorun
(exit (when (equal?
             (normalize-path (find-system-path 'run-file))
             (normalize-path (this-expression-file-name)))
        (run-tests mr-mysql-tests)))
