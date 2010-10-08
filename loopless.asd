(in-package #:cl-user)
(defpackage #:loopless.system
  (:use #:cl #:asdf))
(in-package #:loopless.system)


(defsystem loopless
  :author "Hexstream"
  :components ((:file "loopless")))
