(asdf:defsystem #:ziz
  :description "An ad hoc Quicklisp distribution."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (
    :alexandria
    :hunchentoot
    :ironclad
    :trivial-file-size)
  :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "distribution")))))
