(defpackage #:org.shirakumo.random-state.viewer
  (:use #:cl)
  (:local-nicknames
   (#:random #:org.shirakumo.random-state))
  (:export
   #:generate
   #:generate-all))

(in-package #:org.shirakumo.random-state.viewer)

(defun tempdir ()
  #+windows (merge-pathnames #p"AppData/Local/Temp/" (user-homedir-pathname))
  #+unix #p"/tmp/"
  #-(or windows unix) (user-homedir-pathname))

(defun random-file (generator)
  (make-pathname :name (format NIL "~a-~a" (type-of generator) (get-universal-time))
                 :type "png"
                 :defaults (tempdir)))

(defun generic-open (path)
  #+windows
  (uiop:launch-program (list "explorer.exe" (uiop:native-namestring path)))
  #+(and unix (not darwin))
  (uiop:launch-program (list "xdg-open" (uiop:native-namestring path)))
  #+darwin
  (uiop:launch-program (list "open" (uiop:native-namestring path))))

(defun generate (generator &key file (size 512) (color :grayscale) (open T))
  "Generates a PNG of random samples from the generator.

GENERATOR may be T (for the *GENERATOR*) a RANDOM-STATE, a GENERATOR,
or any other argument permissible for MAKE-GENERATOR.

FILE may be a path to save the file to, or NIL in which case a
temporary file name is chosen for you.

SIZE must be a positive integer designating the dimensions of the
produced image.

COLOR must be either :GRAYSCALE or :TRUECOLOR to decide which type of
color information to produce in the resulting image.

OPEN must be one of:

  NIL    --- Just return the path
  T      --- Open the file in the standard OS viewer
  string --- Pass the path as an argument to the given program

Always returns the actual path the image was written to."
  (let* ((generator (typecase generator
                      ((eql T) random:*generator*)
                      (random:generator generator)
                      (random-state generator)
                      (T (random:make-generator generator))))
         (path (etypecase file
                 (null (random-file generator))
                 (string (pathname file))
                 (pathname file)))
         (data (make-array (* size size (ecase color (:grayscale 1) (:truecolor 3))) :element-type '(unsigned-byte 8)))
         (png (make-instance 'zpng:png :width size :height size :color-type color :image-data data)))
    (dotimes (i (length data))
      (setf (aref data i) (round (* 255 (random:random-unit generator)))))
    (zpng:write-png png path)
    (etypecase open
      (null)
      ((eql T) (generic-open path))
      (string (uiop:run-program (list open (uiop:native-namestring path)))))
    file))

(defun generate-all (&key (directory (tempdir)) (size 512) (color :grayscale))
  "Generates a PNG for all existing generators.

The files are all named after the generator, and placed within
DIRECTORY.

Returns the DIRECTORY.

See GENERATE"
  (dolist (generator (random:list-generator-types) directory)
    (with-simple-restart (contine "Ignore ~a" generator)
      (generate generator :file (make-pathname :name (format NIL "~a" generator) :type "png" :defaults directory)
                          :size size 
                          :color color
                          :open NIL))))
