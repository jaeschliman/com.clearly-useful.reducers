;;;; com.clearly-useful.reducers.asd

(asdf:defsystem #:com.clearly-useful.reducers
  :serial t
  :description "Describe com.clearly-useful.reducers here"
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :license "revised BSD"
  :depends-on (#:com.clearly-useful.protocols
               #:com.clearly-useful.generic-collection-interface)
  :components ((:file "package")
               (:file "com.clearly-useful.reducers")))

