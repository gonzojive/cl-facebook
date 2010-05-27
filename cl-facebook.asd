;;; facebook api for lisp

(defpackage :cl-facebook.system
  (:use :cl :asdf))

(in-package :cl-facebook.system)

(defsystem :cl-facebook
  :name "facebook"
  :author ("Matt Novenstern <mnovenstern@students.colgate.edu>" "Red Daly")
  :version "0.1.0"
  :licence "BSD"
  :description "A library for using the Facebook API."
  :components ((:static-file "cl-facebook.asd")
               (:file "facebook")
               (:file "graph-api" :depends-on ("facebook")))
  :depends-on (:drakma :md5 :cl-json :cl-ppcre :flexi-streams :chunga))
