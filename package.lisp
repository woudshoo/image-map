;;;; package.lisp

(defpackage #:image-map
  (:use #:cl)
  (:export
   :size-to-list
   :point-to-list
   :list-to-size
   :list-to-point
   :make-image-rep
   :make-hi-res-image-rep
   :save-image-rep
   :load-image-rep
   :with-image-rep-context
   :image-rep-with-paked-images
   :make-image-map
   :make-image-rep-with-nssize
   :make-hi-res-image-rep-with-nssize
   :extract-image-from-canvas))

