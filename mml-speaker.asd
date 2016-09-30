;;;; mml-speaker.asd

(asdf:defsystem #:mml-speaker
  :description "Describe mml-speaker here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:esrap #:cl-openal #:cl-alut #:cl-alc)
  :components ((:file "package")
               (:file "parser")
               (:file "player")
               (:file "demos")
               (:file "mml-speaker")))

