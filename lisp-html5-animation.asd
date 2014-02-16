;; Define a config package (used to have a reference in our running application)
(defpackage #:lisp-html5-animation-config (:export #:*base-directory*))
(defparameter lisp-html5-animation-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:lisp-html5-animation
  :serial t
  :description "Foundation HTML5 Animation in Lisp"
  :author "Alvin Francis Dumalus"
  :license "Your license here"
  :depends-on (:RESTAS :RESTAS-DIRECTORY-PUBLISHER :CL-WHO :PARENSCRIPT :SEXML)
  :components ((:file "defmodule")
               (:file "utility")
               (:file "psmacros")
               (:file "parenscripts")
               (:file "classes")
               (:file "instances")
               (:file "lisp-html5-animation")))
