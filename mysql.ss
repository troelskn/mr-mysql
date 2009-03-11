;;;; mr-mysql, MySQL wrappers for PLT Scheme (MzScheme and DrScheme).
;;;; Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva

;;;; This file is part of mr-mysql.
;;;; Mr-mysql is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
;;;; Mr-mysql is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with mr-mysql; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;; Higher level Schemeish MySQL access.
#lang scheme/base

(require "helpers.ss"
         "libmysqlclient.ss"
         (lib "list.ss")
         (lib "setf.ss" "swindle")
         (lib "foreign.ss"))

;; Stack of handlers.
(define handlers null)

;; Add a handler.
(define (push-handler! handler)
  (push! handler handlers) ; Add the handler to the list
  handler)                 ; and return it.

;; Get the current handler.
(define (current-handler)
  (if (empty? handlers)  ; Is there any handlers ?
      null               ; Return empty if so.
      (first handlers))) ; Return the lattest added handler otherwise.

;; Are there any handlers left ?
(define (handlers-left?)
  (> (length handlers) 0)) ; Are there more than 0 handlers ?

;; Pop a handler.
(define (pop-handler!)
  (if (handlers-left?) ; Do we have any handlers ?
      (pop! handlers)  ; If so, pop one.
      null))           ; Otherwise, nevermind.

;; Connect to a MySQL server.
;; Parameters:
;;   #:hostname  hostname to connect to, null or nothing for localhost.
;;   #:username  username to use, null or nothing to use the current (unix) username.
;;   #:password  password to use, null or nothing to use users with empty password.
;;   #:database  database name, null or nothing not to set it yet.
;;   #:port      port to connect to, null or 0 or nothing to use the default.
;;   #:socket    unix socket to connect to, null or nothing to not use it.
;; The client_flag is no supported yet, 0 is used.
;; Returns a mysql handler.
(define/provide (mysql-connect
                 #:hostname (hostname #f)
                 #:username (username #f)
                 #:password (password #f)
                 #:database (database #f)
                 #:port     (port 0)
                 #:socket   (socket #f))
  (let ((mysql-handler (raw-mysql-init #f))) ; Get a mysql handler (initialized).
    (if mysql-handler                        ; Did the initialization work ?
        (let ((connection (raw-mysql-real-connect mysql-handler                     ; Connect to MySQL and save the handler.
                                                  hostname
                                                  username
                                                  password
                                                  database
                                                  port
                                                  socket
                                                  0)))
          (if connection                 ; Did we managed to connect ?
              (push-handler! connection) ; Add the handler to the list of handlers.
              (error 'mysql-connect
                     "Could not connect to MySQL server. Error: (~s) ~s."
                     (raw-mysql-errno mysql-handler)
                     (raw-mysql-error mysql-handler))))
        (error 'mysql-connect ; The initialization did not work (this should be very rare).
               "Not enough memory to allocate MySQL handler"))))

;; Disconnect from a MySQL server. If a handler is passed, that is used to disconnec, otherwise the current handler is used.
;; Parameters:
;; - Handler to disconnet or nothing for current handler.
(define/provide (mysql-disconnect . args)
  (cond
   ((= (length args) 0)                   ; 0 arguments mean: use the current handler.
    (if (handlers-left?)                  ; De wo have any handler left ?
        (mysql-disconnect (pop-handler!)) ; Pop the handler and close the conection.
        (error 'mysql-disconnect          ; There aren't any handlers left!
               "There are not any conections to close.")))
   ((= (length args) 1)                       ; 1 argument means: it is a handler, close it.
    (if (mysql? (first args))                 ; Is it a handler ?
        (raw-mysql-close (first args))        ; Yes, close it.
        (error 'mysql-disconnect              ; No, what are you tring to do ?
               "Expected a mysql handler.")))
   (else (error 'mysql-disconnect "0 or 1 parameter (a mysql handler) expected."))))

;; Disconnect from all the MySQL servers.
(define/provide (mysql-disconnect-all)
  (when (handlers-left?)
    (mysql-disconnect)
    (mysql-disconnect-all)))

;; Run a query and get the result.
;; Parameters:
;; - Query. String with the SQL query to run.
;; - MySQL handle (optional). If not provided, the current handrer is used.
(define/provide (mysql-query query . args)
  (unless (string? query) ; If query is not a string, error!
    (error 'mysql-query "A string was expected as first parameter."))
  (cond
   ((= (length args) 0)                              ; A handler is NOT being specified.
    (if (handlers-left?)                             ; Do we have any handlers left ?
        (mysql-query query (current-handler)) ; Yes, use it!
        (error 'mysql-query                          ; No, error!
               "There aren't any current conections. You need to connect before running a query.")))
   ((= (length args) 1)                                ; A handler is being specified.
    (if (mysql? (first args))                          ; Is the second argument a MySQL handler ?
        (let ((handler (first args)))                  ; Yes, proceed.
          (if (= (raw-mysql-query handler query) 0)    ; Run the query and check if it returns 0 (success).
              (when (> (raw-mysql-field-count handler) ; Is there anything returned ?
                       0)
                (mysql-result->list-of-alists          ; Turn it into a list of alists
                 (raw-mysql-store-result handler)))    ; and return it.
              (error 'mysql-query                      ; The query failed to run.
                     "Query failed. Error: (~s) ~s"    ; Report a nice error.
                     (raw-mysql-errno handler)
                     (raw-mysql-error handler))))
        (error 'mysql-query                            ; The second argument is not a handler.
               "A MySQL handler expected as first argument.")))
   (else
    (error 'mysql-query
           "One (a string) or two (a string and a MySQL handler) arguments where expected, ~s given"
           (length args)))))

(define (mysql-result->list-of-alists result)
  (unless (mysql-result? result) ; If it is not a mysql-result, error!
    (error 'mysql-result->list-of-alists "A mysql result was expected as first argument."))
  (let* ((num-fields (raw-mysql-num-fields result))
         (fields (mysql-fields->list (raw-mysql-fetch-fields result) num-fields)))
    (let process-row ((table '())
                      (row (raw-mysql-fetch-row result)))
      (if row
          (process-row (cons (map cons
                                  (list-of-mysql-fields->list-of-names fields)
                                  (recast-row (mysql-row->list row num-fields)
                                              fields))
                             table)
                       (raw-mysql-fetch-row result))
          (reverse table)))))

(define (recast-row row types)
  (cond
   ((or (empty? row)
        (empty? types))
    null)
   (else
    (cons (case (mysql-field-type (first types))
            ((mysql-type-decimal
              mysql-type-tiny
              mysql-type-short
              mysql-type-long
              mysql-type-float
              mysql-type-double
              mysql-type-timestamp
              mysql-type-longlong
              mysql-type-int24) (string->number (first row)))
            ((mysql-type-null) null)
            ((mysql-type-date
              mysql-type-time
              mysql-type-datetime
              mysql-type-year
              mysql-type-newdate) (first row)) ; TODO: convert dates.
            ((mysql-type-enum
              mysql-type-set) (first row)) ; TODO: handle enum and sets.
            ((mysql-type-tiny-blob
              mysql-type-medium-blob
              mysql-type-long-blob
              mysql-type-blob) (first row)) ; TODO: handle blobs.
            ((mysql-type-var-string
              mysql-type-string) (first row))
            (else (error 'recast-row "Unknown type ~s." (mysql-field-type (first types)))))
          (recast-row (rest row) (rest types))))))

;  ;; Run a query.
;  ;; Parameters:
;  ;; - MySQL handler (optional). If not provided, the current handler is used.
;  ;; - Query. String with the SQL query to run.
;  (define/provide (mysql-query . args)
;    (cond
;      ((= (length args) 1)
;       (if (handlers-left?)
;           (if (string? (first args))
;               (mysql-query (current-handler) (first args))
;               (error 'mysql-query "A string was expected as first argument."))
;           (error 'mysql-query "There aren't any conections.")))
;      ((= (length args) 2)
;       (if (mysql? (first args))
;           (if (string? (second args))
;               (let ((handler (first args))
;                     (query (second args)))
;                 (if (= (raw-mysql-query handler query) 0)
;                     (if (> (raw-mysql-field-count handler) 0)
;                         (push-result! (raw-mysql-store-result handler))
;                         null)
;                     (error 'mysql-query
;                            "Query failed. Error: (~s) ~s"
;                            (raw-mysql-errno (first args))
;                            (raw-mysql-error (second args)))))
;               (error 'mysql-query "A string expected as second argument."))
;           (error 'mysql-query "A MySQL handler expected as first argument.")))
;      (else
;       (error 'mysql-query
;              "One (a string) or two (a mysql handler and a string) arguments where expected, ~s given"
;              (length args)))))
;
;  ;; Having run a query, fetch one row of its output.
;  ;; Parameters:
;  ;; - A result set to get the row from, or nothing to use the last result set obtained.
;  (define/provide (mysql-fetch-row . args)
;    (cond
;      ((= (length args) 0) ; Zero parameters, we have to use the current result.
;       (if (results-left?) ; Check if there's a result.
;           (mysql-fetch-row (current-result)) ; Yes! use it
;           (error 'mysql-fetch-row            ; No! error it.
;                  "No result set present, you need to run a query (that returns a result) first.")))
;      ((= (length args) 1)
;       (let ((result (first args)))
;         ; Checkear if result is mysql-result.
;         (letrec ((num-fields (raw-mysql-num-fields result))
;                  (row (row->list (raw-mysql-fetch-row result) num-fields))
;                  (fields (fields->list (raw-mysql-fetch-fields result) num-fields))
;                  (build-rich-row (lambda (index)
;                                    (cond
;                                      ((>= index num-fields) empty)
;                                      (else
;                                       (cons (cons (mysql-field-name (list-ref fields index))
;                                                   (list-ref row index))
;                                             (build-rich-row (+ index 1))))))))
;           (build-rich-row 0))))
;           ;(list row num-fields (mysql-field-name (list-ref fields 0))))))
;       ;(list
;       ; (row->list (raw-mysql-fetch-row (first args))
;       ;            (raw-mysql-num-fields (first args)))
;       ; (mysql-field-name (raw-mysql-fetch-field-direct (first args) 0))))
;      (else
;       (error 'mysql-fetch-row "0 or 1 argument expected, ~s given." (length args)))))
