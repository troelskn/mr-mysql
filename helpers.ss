;;;; mr-mysql, MySQL wrappers for PLT Scheme (MzScheme and DrScheme).
;;;; Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva

;;;; This file is part of mr-mysql.
;;;; Mr-mysql is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
;;;; Mr-mysql is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with mr-mysql; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;; Some helper useful procedures/macros.
#lang scheme/base

;; Define and provide a variable at the same time.
;; This define has some limitations.
(define-syntax define/provide
  (syntax-rules ()
    ((_ (name . args) body ...)
     (begin (define (name . args) body ...)
            (provide name)))
    ((_ name body ...)
     (begin (define name body ...)
            (provide name)))))
(provide define/provide)
