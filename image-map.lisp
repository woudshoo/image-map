;;;; image-map.lisp

(in-package #:image-map)

;;; "image-map" goes here. Hacks and glory await!

(require "OBJC-SUPPORT")


(defun size-to-list (size &rest rest)
  "Converts an NSSize struct `size' to a list representation.
Will append `rest' to the resulting list."
  (list* (ns:ns-size-width size) (ns:ns-size-height size) rest))

(defun point-to-list (point &rest rest)
  "Converst an NSPoint struct `point' to a list representation.
Will append `rest' to the resulting list."
  (list* (ns:ns-point-x point) (ns:ns-point-y point) rest))

(defun list-to-size (list)
  "Converts a list representation `list' to an NSSize struct.
The first two components of `list' determine the width and height of the 
resulting NSSize."
  (ns:make-ns-size (car list) (second list)))

(defun list-to-point (list)
  "Converts a list representation `list' to an NSPoint struct.
The first two components of  `list' determine the `x' and `y' coordinate
of the resulting NSPoint."
  (ns:make-ns-point (car list) (second list)))

(defun make-image-rep (width height)
  "Create an image representation with pixel dimensions `width' x `height'
This image represention has
- 8 bits per color, 
- an alpha channel (8 bits wide)"
  (let ((width (ceiling width))
	(height (ceiling height)))
    (#/autorelease
     (make-instance 'ns:ns-bitmap-image-rep 
		    :with-bitmap-data-planes ccl:+NULL-PTR+
		    :pixels-wide width
		    :pixels-high height
		    :bits-per-sample 8
		    :samples-per-pixel 4
		    :has-alpha t
		    :is-planar nil
		    :color-space-name #$NSCalibratedRGBColorSpace
		    :bitmap-format 0
		    :bytes-per-row (* 4 width)
		    :bits-per-pixel 32))))

(defun make-image-rep-with-nssize (size)
  (make-image-rep (ns:ns-size-width size) (ns:ns-size-height size)))

(defun make-hi-res-image-rep (width height)
  "Make an image representation for an Retina bitmap with size `width' x `height' in points.
The number of pixels in the image representation is 2x`width' by 2x`height'.

See also `make-image-rep'"
  (let ((rep (make-image-rep (* 2.0 width)
			     (* 2.0 height))))
    (#/setSize: rep (ns:make-ns-size width height))
    rep))

(defun make-hi-res-image-rep-with-nssize (size)
  (make-hi-res-image-rep (ns:ns-size-width size) (ns:ns-size-height size)))

(defun save-image-rep (rep file-name)
  "Save an image representation as an PNG to file-name.
The file will be overwritten if it already exists."
  (objc:with-autorelease-pool
    (objc:with-autoreleased-nsstrings ((ns-file-name file-name))
      (#/writeToFile:atomically:
       (#/representationUsingType:properties: rep #$NSPNGFileType ccl:+NULL-PTR+)
       ns-file-name
       #$NO))))


(defun load-image-rep (file-name)
  "Loads the first image representation from file `file-name'."
    (objc:with-autoreleased-nsstrings 
      ((ns-file-name file-name))
    (#/objectAtIndex: (#/imageRepsWithContentsOfFile: ns:ns-image-rep
						      ns-file-name)
		      0)))


(defmacro with-image-rep-context (image-rep &body body)
  "Runs body while the current graphics context is setup to draw
to the `image-rep'."
  `(unwind-protect
	(progn
	  (#/saveGraphicsState ns:ns-graphics-context)
	  (#/setCurrentContext: ns:ns-graphics-context
				(#/graphicsContextWithBitmapImageRep: ns:ns-graphics-context ,image-rep))
	  ,@body)
     (#/restoreGraphicsState ns:ns-graphics-context)))


(defun draw-image-rep-at-location-in-image-rep (to-draw location canvas)
  "Draw the `to-draw' image representaiton at `location' into the `canvas' image representation.
A location is a list of which the first two values are the `x' and the `y' coordinates."
  (with-image-rep-context canvas
    (#/drawAtPoint: to-draw (list-to-point location))))



(defun rectangles-for-image-file-names (image-file-names)
  "Returns a list of rectangles with the image representation.
The result is a list of which each element is of the form:

  (width height file-name-of-image image-representation)

This format is compatible with the rectangle packing code.

All images which could not be opened are silently ignored.  
"
  (loop :for img-name :in image-file-names
     :for image-rep = (load-image-rep (namestring img-name))
     :unless (ccl:%null-ptr-p image-rep) :collect (size-to-list (#/size image-rep) img-name image-rep)))




(defun image-rep-with-paked-images (image-file-names &key (size (list 512 512)))
  "Pack all images listed in `image-file-names' into an image representation.
The argument `image-file-names' list the file names of the images to be packed.
The `size' argument specifies the size of the resulting image representation.

The result of this function is a cons cell, the car is the image rep containing
the images.  The cdr is contains a list describing the location of the images.

This location list contains for each placed image the following:

 (x y orientation width height image-file-name)

With x and y the location where the image is placed in the resulting image representation.
the width and height are the width and height of the image.  
The orientation is ether :0 or :90 depending if the image is rotated.

Currently only orientation :0 is supported and returned.
"
  (let ((canvas (make-image-rep (first size) (second size))))
    (objc:with-autorelease-pool
      (let ((packed-images (rectangle-packing:pack-rectangles 
			    (rectangle-packing:sort-rectangles-on-size 
			     (rectangles-for-image-file-names image-file-names))
			    :size size)))
	(write-rectangles-to-canvas packed-images canvas)
	(cons canvas
	      (loop :for (x y orientation w h name) :in packed-images
		 :collect (list x y orientation w h name)))))))

(defun make-image-map (image-file-names output-file-name &key (size (list 512 512)))
  "Creates an image map as described in `image-rep-with-paked-images'
and saves the resulting image representation as a PNG at `output-file-name'.

The result of this function is the location list of the `image-rep-with-paked-images'.  
In other words, each placed image is represented in the returned location list as

  (x y orientation width height image-file-name)

with currently orientation always :0."
  (objc:with-autorelease-pool
    (destructuring-bind (canvas . spec) (image-rep-with-paked-images image-file-names :size size)
      (save-image-rep canvas output-file-name)
      spec)))

(defun write-rectangles-to-canvas (rectangles canvas)
  (loop :for (x y orientation width height name image-rep) :in rectangles
     :do
     (draw-image-rep-at-location-in-image-rep image-rep (list x y) canvas)))
