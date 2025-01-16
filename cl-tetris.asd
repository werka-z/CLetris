;;;; -*- Lisp -*-
;;;;
;;;; Clone of the original game a tetris.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>
;;;;
;;;; This file released under some license restrictions,
;;;; see COPYING file.

(defpackage #:cl-tetris-asd
  (:use :cl :asdf))

(in-package #:cl-tetris-asd)

(defsystem cl-tetris
  :name "cl-tetris"
  :version "0.0.1"
  :maintainer "Nedokushev Michael <michael.nedokushev@gmail.com>"
  :author "Nedokushev Michael <michael.nedokushev@gmail.com>"
  :license "MIT (also see COPYING file for details)"
  :description "Yet another 3D Tetris clone"
  :depends-on (#:cl-opengl
               #:lispbuilder-sdl 
               #:cl-glu 
               #:iterate
               #:cl-glut)
  :serial t
  :components ((:file "package")
               (:file "tetris")))
