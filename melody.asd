;;;; melody.asd

(asdf:defsystem #:melody
  :description "Describe melody here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:esrap #:cl-openal #:cl-alut #:cl-alc)
  :components ((:file "package")
               (:file "parser")
               (:file "player")
               (:file "demos")
               (:file "melody")))

