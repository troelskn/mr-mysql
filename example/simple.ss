;;;; mr-mysql, MySQL wrappers for PLT Scheme (MzScheme and DrScheme).
;;;; Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva

;;;; This file is part of mr-mysql.
;;;; Mr-mysql is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
;;;; Mr-mysql is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with mr-mysql; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;; For the tests, a database 'test' must exist with a user named 'test' with password 'test' with full control over the database.
;; WARNING WARNING WARNING
;; These tests are destructive to the 'test' database and should be consider destructive for all the databases (this software is not stable yet).
;; Be carefull and make back ups.
;; WARNING WARNING WARNING

#lang scheme/base
(require "../mysql.ss")

(printf "connecting ...~%")

(define connection (mysql-connect #:hostname "localhost" #:username "root" #:database "test"))

(printf "creating tables ...~%")

(mysql-query "DROP TABLE IF EXISTS `accounts`")
(mysql-query "CREATE TABLE `accounts` (
                 `id` bigint(20) unsigned NOT NULL auto_increment,
                 `username` varchar(255) character set utf8 NOT NULL default '',
                 `password` varchar(255) character set utf8 default NULL,
                 PRIMARY KEY  (`id`),
                 UNIQUE KEY `username` (`username`)
              ) ENGINE=MyISAM DEFAULT CHARSET=utf8 AUTO_INCREMENT=1")

(printf "inserting ...~%")
(mysql-query "INSERT INTO `accounts`
              (`username` , `password` )
              VALUES ('pupeno', 'simplepass')")
(mysql-query "INSERT INTO `accounts`
              (`username` , `password` )
              VALUES ('sandra', 'complexpass')")

(printf "selecting ...~%")
(define foo (mysql-query "SELECT * FROM `accounts`"))

(printf "disconnecting ...~%")
(mysql-disconnect)

(printf "done~%")