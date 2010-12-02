(in-package #:cl-user)
(defpackage #:loopless.system
  (:use #:cl #:asdf))
(in-package #:loopless.system)


(defsystem loopless
  :author "Jean-Philippe Paradis <hexstream@gmail.com>"
  :version "1.0"
  :components ((:file "loopless")))
