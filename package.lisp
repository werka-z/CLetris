;;;; Clone of the original game a tetris.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>
;;;;
;;;; This file released under some license restrictions,
;;;; see COPYING file.

(defpackage #:cl-tetris
  (:use :cl #:iterate)
  (:export #:run #:make-executable))
