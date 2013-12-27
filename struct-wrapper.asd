;;;; struct-wrapper.asd

(asdf:defsystem #:struct-wrapper
    :serial t
    :depends-on (#:cl-html5-parser
                 #:drakma
                 #:cl-ppcre
                 #:basicl)
    :components ((:file "package")
                 (:file "struct-wrapper")))
