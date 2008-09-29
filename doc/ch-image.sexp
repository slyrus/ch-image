((:p
  (:smarkup-metadata 
   (:copyright
    "Copyright 2006-2007, Cyrus Harmon. All Rights Reserved.")
   (:title "Image Processing in Common Lisp with ch-image")
   (:author "Cyrus L. Harmon")
   (:bibtex-database
    "(\"asdf:/ch-image-doc/doc/ch-image-bib\")")
   (:bibtex-style "Science"))
  (:html-metadata  (:htmlcss "simple.css") )

  (:lisp-silent 
   "(asdf:operate 'asdf:load-op 'ch-image)
    (setf smarkup::*baseline-skip* \"14pt\")
    (setf smarkup::*par-skip* \"0pt\")"))

 (:h1 "Abstract")

 (:p "ch-image is an open-source Common Lisp library for image
 representation, processing and manipulation. Along with the clem
 matrix package, ch-image enables common lisp programmers to read
 and write images in a number of different formats, and to
 represent images in a variety of different formats, including
 integers and floating point numbers of various sizes and even
 complex numbers. Furthermore, ch-image provides image processing
 features such as applying affine transformations, interpolation
 of transformed images, and discrete convolution and
 morphological operations on images.")

 (:h1 "Introduction")

 (:p "Common Lisp"
     (:bibcite "steele1990common")
     " is a high-level programming language with a long history
that has been used for many diverse tasks, including artificial
intelligence, computer aided design and drafting, mathematical
computation, and scientific programming. One common complaint
about common lisp is the lack of publicly available libraries for
a wide variety of tasks. In the past few years, projects such as
common-lisp.net"
     (:bibcite "common-lisp-dot-net")
     ", the Common Lisp Gardereners"
     (:bibcite "common-lisp-gardeners")
     ", and The Common Lisp Open Code Collection (CLOCC)"
     (:bibcite "clocc")
     " have been developed to address this lack of publicly
     available libraries. Image representation and manipulation
     is an important, or central, part of many computing tasks,
     but there remains a dearth of feature-rich, efficient,
     publicly available image-processing libraries. ch-image is
     an open-source common lisp library for image processing and
     manipulation and is licensed under a liberal BSD-style
     license. ch-image uses the clem common-lisp matrix package
     which provides for efficient representation and access to
     typed common lisp arrays and for basic matrix operations
     such as matrix arithmetic and affine transformations.")

 (:p "ch-image is designed to be a portable library that can run
 an any standards-compliant common lisp implementation. It has
 been developed entirely using the SBCL"
     (:bibcite "sbcl2006")
     " common lisp implementation and has only been tested on
     SBCL. It should run on other common-lisp implementations,
     however.")

 (:h1 "ch-image Image Representation and Data Structures")
 
 (:p "In ch-image, image data is generally stored in one or a set
 of matrices. The convention of the clem matrix package is to
 store matrix data in row-major order. For ch-image, I have
 chosen to continue to follow this convention, so image data is
 generally refered to in [y,x] coordinates, rather than [x,y]
 coordinates.")

 (:p "Images are stored as instances of class "
     (:code "image")
     ", described below. Pixel intensities are values in
     underlying matrices. In theory these matrices could be
     anything, currently they are all clem matrix instances, of
     various types.")

 (:h2 "image")

 (:p "The fundamental image class is an "
     (:code "image")
     ", which is defined by the following class definition:")

 (:lisp-no-eval 
  "(defclass image ()
  ((data :accessor image-data)
   (height :accessor image-height :initarg :height)
   (width :accessor image-width :initarg :width)
   (channels :accessor image-channels :initform 1)
   (clip-region :accessor clip-region :initarg :clip-region))
  (:documentation \"abstract image class\"))")
 
 (:p "The exported accessors for the image class are "
     (:code "image-data")
     ", "
     (:code "image-height")
     ", "
     (:code "image-width")
     ". The channels slot provides for possible future dynamic
     image channels, but is currently unused. The clip-region is
     currrently only used by the shape drawing library. The
     notion of a single clip-region per image is probably too
     limited and there should probably be something like an image
     context on which shapes are drawn, but this currently does
     not exist.")

 (:h3 "RGB and ARGB images")

 (:p "RGB and ARGB images use three and four channels,
 respectively, to store Red, Green, Blue color intensities and
 ,in the case of ARGB images, Alpha channel
 information. Currently, 8-bit and 16-bit per channel per pixel
 images are supported, via tha rgb-888-image and rgb-hhh-image,
 for 16-bit per channel per pixel images, and the corresponding
 argb image classes.")

 (:h3 "Grayscale Images")

 (:p "The class "
     (:code "gray-image")
     " is the parent class for representations of grayscale
images. Currently, all grayscale images are represented by
subclasses of matrix-gray-image, which in turn subclasses both
gray-image and matrix. Integer valued grayscale images are
represented by the "
     (:code "ub8-")
     ","
     (:code "ub16-")
     ", and "
     (:code "ub32-matrix-image")
     " classes for unsigned integer valued images and sb8, sb16,
and sb32-matrix-image classes for signed integer valued images. "
     (:code "single-float-matrix-image")
     " and"
     (:code "double-float-matrix-image")
     " are used to represent images with floating point
     values. Finally, ")

 (:h2 "Channels")

 (:p "Images have one or more channels that contain pixel value
 information. In the case of RGB and ARGB images, there are 3 and
 4 channels, respectively. Multi-channel images can be subclasses
 of the multichannel-image class to indicate that they have
 multiple channels. The generic function "
     (:code "get-channels")
     " is used to return a list of the channels used by an
     image.")

 (:p "Grayscale images have a single channel. matrix-gray-images
 subclass both gray-image and matrix and act as their own
 image-data. The"
     (:code "get-channels")
     " method of a matrix-gray-image, returns a list containing a
     single item, the matrix-gray-image itself. It may seem
     counter-intuitive to return an image as it's own channel,
     but it simplifies use of the image as a matrix.")

 (:p "The generic function "
     (:code "set-channels")
     " is used to set the channels of an image. In the case of
     matrix-gray-image, Calling set-channels causes the
     underlying matrix storage to point to the specified
     data. FIXME! THIS NEEDS MORE DOCUMENTATION!")

 (:h2 "Pixels")

 (:p "Pixel intensity values are stored in channels as discussed
 above. The generic function "
     (:code "get-pixel")
     " returns the pixel values for an a given row and column of
an image. The actual return values varies depending on the type
of the image. For a multichannel image, "
     (:code "get-pixel")
     " returns a list of the intensites in the channels, while
     for a grayscale image it returns the pixel intensity as a
     single numerical value.")

 (:p "First, lets make an image 200 pixels high and 300 pixels
 wide. Then we will draw a blue background, and a yellow triangle
 on the blue background.")

 (:h2 "Creating an Image")

 (:p "Images can be created by making an instance of the
 appropriate class of images. To create an 8-bit (per channel)
 per pixel RGB image with an alpha channel one would create an
 instance of the "
     (:code "argb-8888-image")
     " class.")

 (:h3 "Sample Image Creation")

 (:p "The following piece of lisp code creates an image 200
 pixels high and 300 pixels wide and fills the image with a
 dark-blue color, using the "
     (:code "fill-image")
     " function, and draws a triangle using the "
     (:code "draw-triangle")
     " function. Finally, the image is saved using the "
     (:code "write-image-file")
     " method which is described in the section about image input
     and output.")

 (:lisp-no-results
  "(defparameter *dark-blue* '(5 5 80))
    (defparameter *example-image-1*
      (let* ((height 200)
             (width 300)
             (img (make-instance 'ch-image:rgb-888-image :height height
                                 :width width))
             (image-pathname \"output-images/example1.png\"))
        (ch-image::fill-image img *dark-blue*)
        (ch-image::draw-triangle img 10 100 50 250 160 70 (list 255 255 20))
        (ensure-directories-exist image-pathname)
        (ch-image:write-image-file image-pathname img)))")

 (:p "This gives us the following image:")

 (:p (:image (:lisp-value "(enough-namestring *example-image-1*)")))

 (:h1 "ch-image I/O")

 (:p "In order to get images in and out of ch-image, I/O routines
 are needed. An initial set of I/O routines are provided by the
 ch-image package, although the package is easily extended to
 support other image formats. Reading and writing images are
 supported to and from the JPEG and TIFF formats. There is
 limited PNG, but only for writing images, which, while still
 limitied, means that ch-image can be used to make PNG files for
 use in serving HTML content.")

 (:p "The read-image-file function locates an image file,
 determines it's type based on the file type (or extension in
 non-lisp parlance), and creates an image of the appropriate
 type (argb or greyscale, and, in theory, of the appropriate
 number of bits, although at this point only 8-bit images are
 supported by the IO routines).")

 (:h1 "Image Processing Functions")

 (:h2 "Image Cropping")

 (:p "First, we load a sample image and crop a region in the
 center. We can also do the affine-transformation and cropping in
 one step, but we'll save that for later.")

 (:lisp
  #q{(defparameter *cropped-salad*
       (let ((img (ch-image:read-image-file "images/salad.jpg")))
         (ch-image::crop-image img :y1 400 :x1 200 :y2 599 :x2 399)))
  (ch-image:write-image-file
   "output-images/salad-cropped.jpg" 
   *cropped-salad*)})

 (:p (:image "output-images/salad-cropped.jpg"))

 (:h2 "Affine Transformation")

 (:p "The function "
     (:code "affine-transform-image")
     " copies applies an affine transformation to a copy of an
image. Note that the original image is unchanged and that a
suitably transformed copy is return. The affine transformation is
specified by an instance of the class"
     
     (:code "clem:affine-transformation")
     ". The coordinates in pixel space of the source and
destination matrices can also be specified via keyword arguments
to "
     (:code "affine-transform-image")
     ".")

 (:h3 "Scaling an Image with an Affine Transformation")

 (:p "We can scale the cropped image from above as follows:")

 (:lisp 
  "(let ((transform (clem:make-affine-transformation
                      :x-scale 1.5d0
                      :y-scale 1.5d0)))
      (let ((bigimg
             (ch-image:affine-transform-image
              *cropped-salad* transform :interpolation :bilinear)))
        (ch-image:write-image-file
         \"output-images/salad-big.jpg\"
         bigimg)))"
  )

 (:p (:image "output-images/salad-big.jpg"))

 (:lisp 
  "(let ((transform
       (clem:make-affine-transformation
        :x-scale 1.2d0
        :y-scale 1.1d0
        :x-shear 1.3d0
        :y-shear 1.8d0
        :theta (* -45d0 (/ 180d0) pi)
        :x-shift 40
        :y-shift 40)))
  (let ((transimg
         (ch-image:affine-transform-image
          *cropped-salad*
          transform :interpolation :bilinear)))
    (ch-image:write-image-file
     \"output-images/salad-trans.jpg\"
     transimg))) ")

 (:p (:image "output-images/salad-trans.jpg"))

 (:p "Notice that the transformed part of the old image fits in
 the new image. The default behavior is to define the extent of
 the new image to completely contain the transformed part of the
 original image. By explicitly specifying the coordinates of the
 source and destination images, one can gain finer control over
 the region of the transformed image that will form the new
 image.")

 (:lisp 
  "(let ((transform
           (clem:make-affine-transformation
            :x-scale 1.2d0
            :y-scale 1.1d0
            :x-shear 1.3d0
            :y-shear 1.8d0
            :theta (* -45d0 (/ 180d0) pi)
            :x-shift 40
            :y-shift 40)))
      (let ((transimg
             (ch-image:affine-transform-image
              *cropped-salad* transform
              :u '(-100 . 100)
              :v '(-100 . 100)
              :x '(-100 . 100)
              :y '(-100 . 100)
              :interpolation :bilinear)))
        (ch-image:write-image-file
         \"output-images/salad-trans2.jpg\"
         transimg)))"
  )

 (:p (:image "output-images/salad-trans2.jpg"))

 (:p "In this example, we scale by a factor of .5")

 (:lisp 
  "(let ((transform
       (clem:make-affine-transformation
        :x-scale .5d0
        :y-scale .5d0)))
  (let ((transimg
         (ch-image:affine-transform-image
          *cropped-salad* transform
          :interpolation :bilinear)))
    (ch-image:write-image-file
     \"output-images/salad-trans3.jpg\"
     transimg)))"
  )

 (:p (:image "output-images/salad-trans3.jpg"))

 (:h3 "Resizing Images")

 (:p "Since affine-transformations are commonly used to resize
 images, a convenience function "
     (:code "resize-image (img y x \\&key (:interpolation :bilinear) (constrain-proportions nil))")
     " is provided.")
 
 (:h3 "Interpolation")

 (:p "When applying an affine transformation to an image, pixels
 in the source image will, in general, not map directly to pixels
 in the destination image. Rather, the pixels will map to
 possibly non-integer positions in the destination image. The
 images represented by ch-image are all discrete images with
 values at integer pixels. Therefore, a mapping from the
 computed, but not necessarily integral, source destination to
 the proper pixel must be performed by interpolation. ch-image
 provides three forms of interpolation, nearest neighbor,
 bilinear interpolation, and quadratic (or bicubic)
 interpolation. Quadratic interpolation will give the best
 results but is the slowest. Nearest-neighbor is the least
 accurate, but is the fastest. Bi-linear interpolation is a good
 compromise and is fairly fast and usually yeilds acceptable
 results.")

 (:h2 "Discrete Convolution")

 (:h3 "Gaussian Blur")

 (:h3 "Sharpen")

 (:h3 "Unsharp Mask")

 (:h2 "Gamma Curves")

 (:h2 "Image Masking")

 (:h2 "Connectivity and Boundaries")

 (:h3 "Labeling Connected Components")

 (:p "For a binary image, connected components can be found using the "
     (:code "label-components")
     " function.")

 (:lisp 
  "(defparameter *shape-image-file* \"output-images/shapes.png\")
    (defparameter *shape-image*
      (let ((img (make-instance 'ch-image::bit-matrix-image
                                :rows 128 :cols 128 :initial-element 0)))
        (ch-image::fill-rectangle img 4 4 10 10 1)
        (ch-image::fill-rectangle img 8 8 16 30 1)
        (ch-image::fill-rectangle img 30 30 50 60 1)
        (ch-image::draw-circle img 75 20 10 1)
        (ch-image::write-image-file
         *shape-image-file*
         (ch-image::make-norm-ub8-image img))
        img))"

  "(defparameter *connected-components*
          (ch-image::label-components *shape-image*
                                      :neighbor-function #'ch-image::8-neighbors))"

  "(defparameter *cc-image-file*
      (ch-image:write-image-file
       \"output-images/connected-components.png\"
       (ch-image::make-norm-ub8-image *connected-components*)))"
  )

 (:p (:image (:lisp-value "(enough-namestring *shape-image-file*)"))
     (:image (:lisp-value "(enough-namestring *cc-image-file*)")))

 (:h3 "Finding Boundaries")

 (:p "Given an image of connected components, one can find the
 internal or external boundaries of a component, given its label,
 using the "
     (:code "component-internal-boundary")
     " and "
     (:code "component-external-boundary")
     " functions.")

 (:lisp 
  "(defparameter *internal-boundary* (ch-image::component-internal-boundary
                                      *connected-components* 3))
    (defparameter *internal-boundary-file*
      (ch-image:write-image-file
       \"output-images/internal-boundary.png\"
       (ch-image::make-norm-ub8-image
        *internal-boundary*)))"

  "(defparameter *external-boundary* (ch-image::component-external-boundary
                                      *connected-components* 3))
    (defparameter *external-boundary-file*
      (ch-image:write-image-file
       \"output-images/external-boundary.png\"
       (ch-image::make-norm-ub8-image
        *external-boundary*)))"

  "(defparameter *boundary* (ch-image::component-boundary
                             *connected-components* 3))
    (defparameter *boundary-file*
      (ch-image:write-image-file
       \"output-images/boundary.png\"
       (ch-image::make-norm-ub8-image
        *boundary*)))"
  )

 (:p
  (:image (:lisp-value "(enough-namestring *internal-boundary-file*)"))
  (:image (:lisp-value "(enough-namestring *external-boundary-file*)"))
  (:image (:lisp-value "(enough-namestring *boundary-file*)")))
 
 (:p "We see that the external boundary of the non-filled circle
 is in fact a double-ring around the circle. If we only wanted
 the exterior circle, we would need to fill in the object
 first.")

 (:h2 "Morphological Operations")

 (:h3 "Dilate")

 (:h3 "Erode")

 (:h2 "Fast Fourier Transform")

 (:p "The Fast Fourier Transform of images is provided using a
 Foreign-function Interface (FFI) to the FFTW (Fastest Fourier
 Transfrom in the West) library"
     (:bibcite "frigo1998fftw")
     ". A number of utility functions are provided to convert
     between the ch-image image representation of fftw matrix
     represenations and to make images of the magnitude and
     frequency of transformed images, and to reconstruct
     spatial-domain images from magnitude and frequency images.")

 (:h2 "Image Copying and Conversion")

 (:h1 "Shape Drawing Primitives")

 (:p "ch-image contains a small number of drawing primitive
 routines that can be used to try shapes on an image. Currently,
 lines, circles, rectangles, triangles and polygons are
 supported.")

 (:h2 "Lines")

 (:h2 "Circles")

 (:h2 "Rectangles")

 (:h2 "Triangles")

 (:h2 "Polygons")

 (:h1 "Using the ch-image APIs")

 (:h2 "Example 1 - Loading an Image from a JPEG File")

 (:p "We use read-image-file to load a sample image of the San
 Francisco skyline:")

 (:p "Here's the original image:")

 (:lisp 
  "(defparameter *sanfran*
      (ch-image:read-image-file \"images/sanfran.jpg\"))"
  )

 (:p (:image (:lisp-value #q{(namestring (ch-asdf:asdf-lookup-path "asdf:/ch-image-doc/doc/images/sanfran"))})))

 (:h2 "Example 2 - Convert Image to Grayscale and Write PNG File")

 (:p "The following code will convert the image to a grayscale
 image and write it out as a PNG file:")

 (:lisp 
  "(defparameter *sanfran-png-file*
      (ch-image:write-image-file
       \"output-images/sanfran.png\"
       (ch-image:argb-image-to-gray-image *sanfran*)))"
  )

 (:p (:image (:lisp-value "(enough-namestring *sanfran-png-file*)")))

 (:h2 "Example 3 - Applying a gamma curve")

 (:lisp 
  "(defparameter *sanfran-lighter-image*
      (ch-image::apply-gamma *sanfran* 0.60d0))
    (defparameter *sanfran-lighter-file*
      (ch-image:write-image-file
       \"output-images/sanfran-lighter.jpg\"
       *sanfran-lighter-image*))"
  )

 (:p (:image (:lisp-value "(enough-namestring *sanfran-lighter-file*)")))

 (:h2 "Example 4 - Circles")

 (:lisp 
  "(defparameter *circles*
      (let* ((width 500)
             (height 250)
             (img (make-instance 'ch-image:rgb-888-image :width width :height height)))
        (dotimes (i 100)
          (let ((radius (random 50))
                (y (+ 50 (random (- height 100))))
                (x (+ 50 (random (- width 100))))
                (color (list (random 255) (random 255) (random 255))))
            (if (> (random 2) 0)
                (ch-image:draw-circle img y x radius color)
                (ch-image:fill-circle img y x radius color))))
        (ch-image:write-image-file
         \"output-images/circles.png\"
         img)))"
  )

 (:p (:image (:lisp-value "(enough-namestring *circles*)")))

 (:h1 "ch-image API reference")

 (:h1 "Installing ch-image and required packages")

 (:bibliography))
