;;;; mr-mysql, MySQL wrappers for PLT Scheme (MzScheme and DrScheme).
;;;; Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva

;;;; This file is part of mr-mysql.
;;;; Mr-mysql is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
;;;; Mr-mysql is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with mr-mysql; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;; Low level access to the MySQL API. At these level we are still dealing with dangerous C stuff, like pointers. Use with care or do not use.
#lang scheme/base

;; /usr/include/mysql/mysql.h

(require "helpers.ss"
         (lib "foreign.ss"))

;; Hopefully, this is not sex, only code.
(unsafe!)

;; The library
(define libmysqlclient (ffi-lib "libmysqlclient"))

;; MySQL handler.
(define _mysql (_cpointer/null 'mysql (make-ctype _pointer #f #f)))

;; Predicate to check if an object is a MySQL handler.
(define/provide mysql?
  (lambda (m)
    (cpointer-has-tag? m 'mysql)))

;; MySQL result representation.
(define _mysql-result (_cpointer/null 'mysql-result (make-ctype _pointer #f #f)))

;; Predicate to check if an object is a MySQL result.
(define/provide mysql-result?
  (lambda (m)
    (cpointer-has-tag? m 'mysql-result)))

;; MySQL row representation.
(define _mysql-row (_cpointer/null 'mysql-row (make-ctype _pointer #f #f)))

(define _field-types
  (_enum '(mysql-type-decimal
           mysql-type-tiny
           mysql-type-short
           mysql-type-long
           mysql-type-float
           mysql-type-double
           mysql-type-null
           mysql-type-timestamp
           mysql-type-longlong
           mysql-type-int24
           mysql-type-date
           mysql-type-time
           mysql-type-datetime
           mysql-type-year
           mysql-type-newdate
           mysql-type-enum = 247
           mysql-type-set = 248
           mysql-type-tiny-blob = 249
           mysql-type-medium-blob = 250
           mysql-type-long-blob = 251
           mysql-type-blob = 252
           mysql-type-var-string = 253
           mysql-type-string = 254
           mysql-type-geometry = 255)))

;; http://dev.mysql.com/doc/mysql/en/c-api-datatypes.html
;; This structure contains information about a field, such as the field's name, type, and size. Its members are described in more detail here. You may obtain the MYSQL_FIELD structures for each field by calling mysql_fetch_field() repeatedly. Field values are not part of this structure; they are contained in a MYSQL_ROW structure.
(define-cstruct _mysql-field
  ((name _string)
   (org-name _string)
   (table _string)
   (org-table _string)
   (db _string)
   (catalog _string)
   (def _string)
   (length _ulong)
   (max-length _ulong)
   (name-length _uint)
   (org-name-length _uint)
   (table-length _uint)
   (org-table-length _uint)
   (db-length _uint)
   (catalog-length _uint)
   (def-length _uint)
   (flags _uint)
   (decimals _uint)
   (charsetnr _uint)
   (type _field-types)))
(provide _mysql-field)
(provide _mysql-field-pointer)
(provide mysql-field-type)

;; Turn an array of fields into a Scheme List.
;; Parameters:
;; - fields: the pointer to the first field in the array.
;; - num-fields: how many fields there are.
(define/provide (mysql-fields->list fields num-fields)
  (cvector->list (make-cvector* fields _mysql-field num-fields)))

;; Turn a list of MySQL fields into a list of the name of the fields.
;; Parameters:
;; - list-of-fields: the list of fields.
(define/provide (list-of-mysql-fields->list-of-names list-of-fields)
  (map mysql-field-name list-of-fields))

;; Turn a list of MySQL fields into a list of the type of the fields.
;; Parameters:
;; - list-of-fields: the list of fields.
(define/provide (list-of-mysql-fields->list-of-types list-of-fields)
  (map mysql-field-type list-of-fields))

;; Turn a MySQL row into a list.
;; Parameters:
;; - row: the mysql row.
;; - num-fields: how many fields there are.
(define/provide (mysql-row->list row num-fields)
  (cvector->list (make-cvector* row _string num-fields)))

(define _my-ulongulong
  (make-ctype _uint64 #f #f))

(define _my-bool
  (make-ctype _byte
              (lambda (x) (if x 1 0))
              (lambda (x) (not (eq? 0 x)))))

(define _mysql-field-offset
  (make-ctype _uint #f #f))

(define _mysql-option (_enum '(mysql-opt-connect-timeout
                               mysql-opt-compress
                               mysql-opt-named-pipe
                               mysql-init-command
                               mysql-read-default-file
                               mysql-read-default-group
                               mysql-set-charset-dir
                               mysql-set-charset-name
                               mysql-opt-local-infile
                               mysql-opt-protocol
                               mysql-shared-memory-base-name
                               mysql-opt-read-timeout
                               mysql-opt-write-timeout
                               mysql-opt-use-result
                               mysql-opt-use-remote-connection
                               mysql-opt-use-embedded-connection
                               mysql-opt-guess-connection
                               mysql-set-client-ip
                               mysql-secure-auth)))
(define _mysql-set-option (_enum '(mysql-option-multi-statements-on
                                   mysql-option-multi-statements-off)))

;; (_bitmask '(mysql-shutdown-killable-connect = 1
;;                                             mysql-shutdown-killable-trans = 2
;;                                             mysql-shutdown-killable-lock-table = 4
;;                                             mysql-shutdown-killable-update = 8))

(define _mysql-shutdown-level
  (_enum '(shutdown-default = 0
                            shutdown-wait-connections = 1 ; mysql-shutdown-killable-connect
                            shutdown-wait-transactions = 2 ; mysql-shutdown-killable-trans
                            shutdown-wait-updates = 8 ; mysql-shutdown-killable-update
                            shutdown-all-buffers = 16 ; mysql-shutdown-killable-update << 1
                            shutdown-wait-critical-buffers = 17 ;(mysql-shutdown-killable-update << 1)+1
                            kill-query = 254
                            kill-connection = 255)))

(define raw-mysql-affected-rows
  (get-ffi-obj "mysql_affected_rows" libmysqlclient (_fun _mysql -> _my-ulongulong)))
(define raw-mysql-change-user
  (get-ffi-obj "mysql_change_user" libmysqlclient (_fun _mysql _string _string _string -> _my-bool)))
(define raw-mysql-character-set-name
  (get-ffi-obj "mysql_character_set_name" libmysqlclient (_fun _mysql -> _string)))

;; Close a previously stablished conection to a MySQL server.
;; Parameters:
;; - MySQL handler.
;; http://dev.mysql.com/doc/mysql/en/mysql-close.html
;; void mysql_close(MYSQL *mysql)
(define/provide raw-mysql-close
  (get-ffi-obj "mysql_close" libmysqlclient (_fun _mysql -> _void)))

;  (define raw-mysql-data-seek
;    (get-ffi-obj "mysql_data_seek" libmysqlclient (_fun _mysql-result _my-ulongulong -> _void)))
;  (define raw-mysql-debug
;    (get-ffi-obj "mysql_debug" libmysqlclient (_fun _string -> _void)))
;  (define raw-mysql-dump-debug-info
;    (get-ffi-obj "mysql_dump_debug_info" libmysqlclient (_fun _mysql -> _int)))

;; http://dev.mysql.com/doc/mysql/en/mysql-errno.html
;; unsigned int mysql_errno(MYSQL *mysql)
(define/provide raw-mysql-errno
  (get-ffi-obj "mysql_errno" libmysqlclient (_fun _mysql -> _uint)))

;; http://dev.mysql.com/doc/mysql/en/mysql-error.html
;; const char *mysql_error(MYSQL *mysql)
(define/provide raw-mysql-error
  (get-ffi-obj "mysql_error" libmysqlclient (_fun _mysql -> _string)))

;  (define raw-mysql-fetch-field
;    (get-ffi-obj "mysql_fetch_field" libmysqlclient (_fun _mysql-result -> _mysql-field-pointer)))

;; http://dev.mysql.com/doc/mysql/en/mysql-fetch-fields.html
;; MYSQL_FIELD *mysql_fetch_fields(MYSQL_RES *result)
(define/provide raw-mysql-fetch-fields
  (get-ffi-obj "mysql_fetch_fields" libmysqlclient (_fun _mysql-result -> _mysql-field-pointer)))

;  (define raw-mysql-fetch-field-direct
;    (get-ffi-obj "mysql_fetch_field_direct" libmysqlclient (_fun _mysql-result _uint -> _mysql-field-pointer)))
;  (define raw-mysql-fetch-lengths
;    (get-ffi-obj "mysql_fetch_lengths" libmysqlclient (_fun _mysql-result -> _pointer)))

;; http://dev.mysql.com/doc/mysql/en/mysql-fetch-row.html
;; MYSQL_ROW mysql_fetch_row(MYSQL_RES *result)
(define/provide raw-mysql-fetch-row
  (get-ffi-obj "mysql_fetch_row" libmysqlclient (_fun _mysql-result -> _mysql-row)))

;; http://dev.mysql.com/doc/mysql/en/mysql-field-count.html
;; unsigned int mysql_field_count(MYSQL *mysql)
(define/provide raw-mysql-field-count
  (get-ffi-obj "mysql_field_count" libmysqlclient (_fun _mysql -> _uint)))

;  (define raw-mysql-field-seek
;    (get-ffi-obj "mysql_field_seek" libmysqlclient (_fun _mysql-result _mysql-field-offset -> _mysql-field-offset)))
;  (define raw-mysql-field-tell
;    (get-ffi-obj "mysql_field_tell" libmysqlclient (_fun _mysql-result -> _mysql-field-offset)))
;  (define raw-mysql-free-result
;    (get-ffi-obj "mysql_free_result" libmysqlclient (_fun _mysql-result -> _void)))
;TODO: find out what is MY_CHARSET_INFO
;  (define raw-mysql-get-character-set-info
;    (get-ffi-obj "mysql_get_character_set_info" libmysqlclient (_fun _mysql -> )))
;  (define raw-mysql-get-client-info
;    (get-ffi-obj "mysql_get_client_info" libmysqlclient (_fun -> _string)))
;  (define raw-mysql-get-client-version
;    (get-ffi-obj "mysql_get_client_version" libmysqlclient (_fun -> _ulong)))
;  (define raw-mysql-get-host-info
;    (get-ffi-obj "mysql_get_host_info" libmysqlclient (_fun _mysql -> _string)))
;  (define raw-mysql-get-proto-info
;    (get-ffi-obj "mysql_get_proto_info" libmysqlclient (_fun _mysql -> _uint)))
;  (define raw-mysql-get-server-info
;    (get-ffi-obj "mysql_get_server_info" libmysqlclient (_fun _mysql -> _string)))
;  (define raw-mysql-get-server-version
;    (get-ffi-obj "mysql_get_server_version" libmysqlclient (_fun _mysql -> _ulong)))
;  (define raw-mysql-hex-string
;    (get-ffi-obj "mysql_hex_string" libmysqlclient (_fun _string _string _ulong -> _ulong)))
;  (define raw-mysql-info
;    (get-ffi-obj "mysql_info" libmysqlclient (_fun _mysql -> _string)))

;; http://dev.mysql.com/doc/mysql/en/mysql-init.html
;; MYSQL *mysql_init(MYSQL *mysql)
(define/provide raw-mysql-init
  (get-ffi-obj "mysql_init" libmysqlclient (_fun _mysql -> _mysql)))

;  (define raw-mysql-insert-id
;    (get-ffi-obj "mysql_insert_id" libmysqlclient (_fun _mysql -> _my-ulongulong)))
;  (define raw-mysql-kill
;    (get-ffi-obj "mysql_kill" libmysqlclient (_fun _mysql _ulong -> _int)))
;  (define raw-mysql-library-init
;    (get-ffi-obj "mysql_library_init" libmysqlclient (_fun _int _pointer _pointer -> _int)))
;  (define raw-mysql-library-end
;    (get-ffi-obj "mysql_library_end" libmysqlclient (_fun _void -> _void)))
;  (define raw-mysql-list-dbs
;    (get-ffi-obj "mysql_list_dbs" libmysqlclient (_fun _mysql _string -> _mysql-result)))
;  (define raw-mysql-list-fields
;    (get-ffi-obj "mysql_list_fields" libmysqlclient (_fun _mysql _string _string -> _mysql-result)))
;  (define raw-mysql-list-processes
;    (get-ffi-obj "mysql_list_processes" libmysqlclient (_fun _mysql -> _mysql-result)))
;  (define raw-mysql-list-tables
;    (get-ffi-obj "mysql_list_tables" libmysqlclient (_fun _mysql _string -> _mysql-result)))

;; http://dev.mysql.com/doc/mysql/en/mysql-num-fields.html
;; unsigned int mysql_num_fields(MYSQL_RES *result)
(define/provide raw-mysql-num-fields
  (get-ffi-obj "mysql_num_fields" libmysqlclient (_fun _mysql-result -> _uint)))

;  (define mysql_num_rows
;    (get-ffi-obj "mysql_num_rows" libmysqlclient (_fun _mysql-result -> _my-ulongulong)))
;  (define mysql_options
;    (get-ffi-obj "mysql_options" libmysqlclient (_fun _mysql _mysql-option _string -> _int)))
;  (define mysql_ping
;    (get-ffi-obj "mysql_ping" libmysqlclient (_fun _mysql -> _int)))

;; http://dev.mysql.com/doc/mysql/en/mysql-query.html
;; int mysql_query(MYSQL *mysql, const char *query)
; (define/provide raw-mysql-query
;   (get-ffi-obj "mysql_query" libmysqlclient (_fun _mysql _string -> _int)))

;; http://dev.mysql.com/doc/refman/5.1/en/mysql-real-query.html
;; int mysql_real_query(MYSQL *mysql, const char *stmt_str, unsigned long length)
(define/provide raw-mysql-real-query
  (get-ffi-obj "mysql_real_query" libmysqlclient (_fun _mysql _string _ulong -> _int)))

;; http://dev.mysql.com/doc/mysql/en/mysql-real-connect.html
;; MYSQL *mysql_real_connect(MYSQL *mysql, const char *host, const char *user, const char *passwd, const char *db, unsigned int port, const char *unix_socket, unsigned long client_flag)
(define/provide raw-mysql-real-connect
  (get-ffi-obj "mysql_real_connect" libmysqlclient
               (_fun _mysql _string _string _string
                     _string _uint _string _ulong -> _mysql)))

;; http://dev.mysql.com/doc/refman/5.1/en/mysql-real-escape-string.html
;; unsigned long mysql_real_escape_string(MYSQL *mysql, char *to, const char *from, unsigned long length)
(define/provide raw-mysql-real-escape-string
  (get-ffi-obj "mysql_real_escape_string" libmysqlclient (_fun _mysql _pointer _string _ulong -> _ulong)))

;  (define raw-mysql-refresh
;    (get-ffi-obj "mysql_refresh" libmysqlclient (_fun _mysql _uint -> _int)))
;  (define raw-mysql-reload
;    (get-ffi-obj "mysql_reload" libmysqlclient (_fun _mysql -> _int)))
;  (define raw-mysql-row-seek
;    (get-ffi-obj "mysql_row_seek" libmysqlclient (_fun _mysql-result _pointer -> _pointer)))
;  (define raw-mysql-row-tell
;    (get-ffi-obj "mysql_row_tell" libmysqlclient (_fun _mysql-result -> _pointer)))
;  (define raw-mysql-select-db
;    (get-ffi-obj "mysql_select_db" libmysqlclient (_fun _mysql _string -> _int)))
;  (define raw-mysql-set-character-set
;    (get-ffi-obj "mysql_set_character_set" libmysqlclient (_fun _mysql _string -> _int)))
;  (define raw-mysql-set-server-option
;    (get-ffi-obj "mysql_set_server_option" libmysqlclient (_fun _mysql _mysql-set-option -> _int)))
;  (define raw-mysql-shutdown
;    (get-ffi-obj "mysql_shutdown" libmysqlclient (_fun _mysql _mysql-shutdown-level -> _int)))
;  (define raw-mysql-sqlstate
;    (get-ffi-obj "mysql_sqlstate" libmysqlclient (_fun _mysql -> _string)))
;  (define raw-mysql-ssl-set
;    (get-ffi-obj "mysql_ssl_set" libmysqlclient (_fun _mysql _string _string _string _string _string -> _int)))
;  (define raw-mysql-stat
;    (get-ffi-obj "mysql_stat" libmysqlclient (_fun _mysql -> _string)))

;; http://dev.mysql.com/doc/mysql/en/mysql-store-result.html
;; MYSQL_RES *mysql_store_result(MYSQL *mysql)
(define/provide raw-mysql-store-result
  (get-ffi-obj "mysql_store_result" libmysqlclient (_fun _mysql -> _mysql-result)))

;  (define raw-mysql-thread-id
;    (get-ffi-obj "mysql_thread_id" libmysqlclient (_fun _mysql -> _ulong)))
;  (define raw-mysql-use-result
;    (get-ffi-obj "mysql_use_result" libmysqlclient (_fun _mysql -> _mysql-result)))
;  (define raw-mysql-warning-count
;    (get-ffi-obj "mysql_warning_count" libmysqlclient (_fun _mysql -> _uint)))
;  (define raw-mysql-commit
;    (get-ffi-obj "mysql_commit" libmysqlclient (_fun _mysql -> _my-bool)))
;  (define raw-mysql-rollback
;    (get-ffi-obj "mysql_rollback" libmysqlclient (_fun _mysql -> _my-bool)))
;  (define raw-mysql-autocommit
;    (get-ffi-obj "mysql_autocommit" libmysqlclient (_fun _mysql _my-bool -> _my-bool)))
;  (define raw-mysql-more-results
;    (get-ffi-obj "mysql_more_results" libmysqlclient (_fun _mysql -> _my-bool)))
;  (define raw-mysql-next-result
;    (get-ffi-obj "mysql_next_result" libmysqlclient (_fun _mysql -> _int)))
