;;;; image-map.asd

(asdf:defsystem #:image-map
  :serial t
  :description "Creates an image packing multiple smaller images.  Useful for creating texture maps for OpenGL."
  :author "Willem Rein Oudshoorn <woudshoo@xs4all.nl>"
  :version 0.1
  :license "LLGPL, but I am flexible, ask me if you want something else."
  :depends-on (#:rectangle-packing #:ccl)
  :components ((:file "package")
               (:file "image-map")))

