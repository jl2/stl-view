;;;; stl-view.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:stl-view
  :description "Describe stl-view here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:lispbuilder-sdl
               #:cl-opengl
               #:cl-glu
               #:stl)
  :serial t
  :components ((:file "package")
               (:file "stl-view")))

