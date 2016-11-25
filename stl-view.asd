;;;; stl-view.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:stl-view
  :description "Describe stl-view here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:sdl2
               #:cl-opengl
               #:cl-glu
               #:stl
               #:trivial-main-thread)
  :serial t
  :components ((:file "package")
               (:file "stl-view")))

