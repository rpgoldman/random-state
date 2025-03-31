(in-package #:org.shirakumo.random-state)

(defvar *generator-types* '(random-state))
(defvar *generators* (make-hash-table :test 'eql))

(defun global-generator (name)
  (or (gethash name *generators*)
      (setf (gethash name *generators*)
            (make-generator name))))

(defun (setf global-generator) (value name)
  (setf (gethash name *generators*) value))

#-allegro
(define-compiler-macro global-generator (&whole whole name &environment env)
  (if (constantp name env)
      `(load-time-value (or (gethash ,name *generators*)
                            (setf (gethash ,name *generators*)
                                  (make-generator ,name))))
      whole))

(defmacro define-generator-fun (name (gen &rest args))
  (let ((argsyms (loop for arg in args unless (find arg LAMBDA-LIST-KEYWORDS) collect arg)))
    `(progn (defgeneric ,name (,gen ,@args))

            (define-compiler-macro ,name (&whole whole ,gen ,@args &environment env)
              (if (constantp ,gen env)
                  `(,',name (ensure-generator ,,gen) ,,@argsyms)
                  whole))

            (defmethod ,name ((,gen symbol) ,@args)
              (,name (global-generator ,gen) ,@argsyms)))))

(defun list-generator-types ()
  (setf *generator-types* (sort *generator-types* #'string<)))

(defstruct (generator
            (:constructor NIL)
            (:copier NIL)
            (:conc-name NIL))
  (%seed 0 :type (unsigned-byte 64)))

(defmethod print-object ((generator generator) stream)
  ;; Fully printing a state can be huge, so unless readability is requested,
  ;; use P-U-O to print something shorter.
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (generator stream :type T)
        (format stream "~s" (seed generator))))
  ;; P-U-O does not return the object, which PRINT-OBJECT is supposed to do.
  generator)

(defmethod make-load-form ((object generator) &optional env)
  (make-load-form-saving-slots object :environment env))

(defun ensure-generator (generator-ish)
  (etypecase generator-ish
    (symbol (global-generator generator-ish))
    (random-state generator-ish)
    (generator generator-ish)))

(define-compiler-macro ensure-generator (&whole whole generator-ish &environment env)
  (if (constantp generator-ish env)
      `(load-time-value (etypecase ,generator-ish
                          (symbol (global-generator ,generator-ish))
                          (random-state ,generator-ish)
                          (generator ,generator-ish)))
      whole))

(defgeneric %make-generator (type &key))
#+sbcl
(locally (declare (sb-ext:muffle-conditions style-warning))
  (define-generator-fun seed (generator)))
#-sbcl (define-generator-fun seed (generator))
(define-generator-fun reseed (generator new-seed))
(define-generator-fun next-byte (generator))
(define-generator-fun bits-per-byte (generator))
(define-generator-fun copy (generator))

(defun multivariate-p (generator)
  (listp (bits-per-byte generator)))

;;; supporting methods for COPY
(defmethod copy ((thing number))
  thing)

(defmethod copy ((thing array))
  (make-array (array-dimensions thing)
              :element-type (array-element-type thing)
              :fill-pointer (array-has-fill-pointer-p thing)
              :adjustable (adjustable-array-p thing)
              :initial-contents thing))


(defun make-generator (type &optional (seed T) &rest initargs)
  (let ((generator (apply #'%make-generator type initargs)))
    (when seed (reseed generator seed))
    generator))

(defmethod reseed ((generator generator) (new-seed (eql T)))
  (reseed generator (hopefully-sufficiently-random-seed))
  generator)

(defmethod seed ((generator generator))
  (%seed generator))

(defmacro define-generator (name bits-per-byte super slots &body bodies)
  (let* ((constructor (intern* 'make name))
         (copy (intern* 'copy name))
         (reseed (intern* name 'reseed))
         (next (intern* name 'next))
         (hash (intern* name 'hash))
         (generator (intern* 'generator))
         (bindings (append (loop for (slot) in slots collect
                                 `(,slot (,(intern* name slot) ,generator)))
                           (loop for (slot) in (rest super) collect
                                 `(,slot (,(intern* (first super) slot) ,generator)))))
         (seed (intern* 'seed))
         (index (intern* 'index)))
    `(progn
       (pushnew ',name *generator-types*)

       (defstruct (,name
                   (:include ,@super)
                   (:constructor ,constructor)
                   (:copier NIL)
                   (:predicate NIL))
         ,@slots)

       ,@(loop for (type . body) in bodies
               collect (ecase type
                         (:copy
                          `(defun ,copy (,generator)
                             ,@body))
                         (:reseed
                          `(progn (defun ,reseed (,generator ,seed)
                                    (setf (%seed ,generator) ,seed)
                                    (symbol-macrolet ,bindings
                                      ,@body))
                                  (defmethod reseed ((,generator ,name) (new-seed integer))
                                    (,reseed ,generator new-seed))))
                         (:next
                          `(progn (defun ,next (,generator)
                                    ;; sometimes argument is not used.
                                    (declare (ignorable ,generator))
                                    (symbol-macrolet ,bindings
                                      ,@body))
                                  (defmethod next-byte ((,generator ,name))
                                    (,next ,generator))
                                  (defmethod next-byte-fun ((,generator ,name))
                                    #',next)))
                         (:hash
                          `(progn (defun ,hash (,index ,seed ,@(mapcar #'first bindings))
                                    (declare (type (unsigned-byte 64) ,index ,seed))
                                    (declare ,@(loop for (slot nil . args) in slots
                                                     collect `(type ,(getf args :type T) ,slot)))
                                    ,@body)
                                  (defmethod hash ((,generator ,name) ,index ,seed)
                                    (,hash ,index ,seed ,@(mapcar #'second bindings)))
                                  (defun ,next (,generator)
                                    (let ((index (fit-bits 64 (1+ (,(intern* name 'index) ,generator))))
                                          (seed (,(intern* name '%seed) ,generator)))
                                      (setf (,(intern* name 'index) ,generator) index)
                                      (symbol-macrolet ,bindings
                                        (locally
                                            ,@body))))
                                  (defmethod next-byte ((,generator ,name))
                                    (,next ,generator))
                                  (defmethod next-byte-fun ((,generator ,name))
                                    #',next)))))

       ,@(unless (find :copy bodies :key #'car)
           `((defun ,copy (,generator)
               ;; sometimes the argument is not used.
               (declare (ignorable ,generator))
               (,constructor ,@(loop for binding in bindings
                                     collect (intern (string (first binding)) "KEYWORD")
                                     collect `(copy ,(second binding)))))))
       (defmethod copy ((generator ,name))
         (,copy generator))
       (defmethod %make-generator ((type (eql ',name)) &rest initargs &key &allow-other-keys)
         (apply #',constructor initargs))
       (defmethod bits-per-byte ((generator ,name))
         ,bits-per-byte))))

(defstruct (stateful-generator
            (:include generator)
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)))

(defmethod %make-generator :around ((type stateful-generator) &key)
  (let ((generator (call-next-method)))
    (reseed generator 0)
    generator))

(defmethod %make-generator ((type symbol) &rest args)
  (let ((name (find-symbol (string type) #.*package*)))
    (if (and name (member name *generator-types*))
        (apply #'%make-generator name args)
        (error "No generator with name ~s known." type))))

(defstruct (hash-generator
            (:include generator)
            (:constructor NIL)
            (:copier NIL)
            (:predicate NIL)
            (:conc-name NIL))
  (index 0 :type (unsigned-byte 64)))

(define-generator-fun rewind (hash-generator &optional by))
(define-generator-fun hash (hash-generator index seed))

(defmethod reseed ((generator hash-generator) (seed integer))
  (setf (index generator) 0)
  (setf (%seed generator) (fit-bits 64 seed)))

(defmethod rewind ((generator hash-generator) &optional (by 1))
  (setf (index generator) (fit-bits 64 (- (index generator) by))))
