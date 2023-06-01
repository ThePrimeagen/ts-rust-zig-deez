(cl:defpackage #:deez/runtime
  (:export
   #:|let|
   #:|if|
   #:|else|
   #:|return|
   #:|fn|
   #:|true|
   #:|false|
   #:!
   #:+
   #:-
   #:*
   #:/
   #:<
   #:>
   #:==
   #:!=
   #:|len|
   #:|push|
   #:|first|
   #:|rest|
   #:|puts|))
(cl:in-package #:deez/runtime)

;; CL is a Lisp-2, Monkeylang isn't, the solution to this is to define functions
;; in terms of global variables and let the evaluator FUNCALL them.
(cl:defmacro defun (symbol lambda-list cl:&body body)
  `(cl:defparameter ,symbol (cl:lambda ,lambda-list
                              ,@body)))

(cl:defparameter |true| '|true|)
(cl:defparameter |false| '|false|)

(cl:defparameter ! #'cl:not)
(cl:defparameter - #'cl:-)
(cl:defparameter * #'cl:*)
(cl:defparameter / #'cl:/)
(cl:defparameter < #'cl:<)
(cl:defparameter > #'cl:>)
(cl:defparameter == #'cl:equal)

(defun != (left right)
  (cl:not (cl:equal left right)))

(defun + (left right)
  (cl:cond
    ((cl:or (cl:typep left 'cl:string)
            (cl:typep right 'cl:string))
     (cl:format cl:nil "~A~A" left right))
    ((cl:and (cl:typep left 'cl:number)
             (cl:typep right 'cl:number))
     (cl:+ left right))
    (cl:t (cl:error "Type error ~A + ~A" (cl:type-of left) (cl:type-of right)))))

(cl:defparameter |len| #'cl:length)

(defun |push| (array elem)
  (cl:vector-push-extend elem array)
  array)

(defun |first| (array)
  (cl:if (cl:= (cl:length array) 0)
         #()
         (cl:aref array 0)))

(defun |rest| (array)
  (cl:if (cl:< (cl:length array) 2)
         #()
         (cl:subseq array 1)))

(defun |puts| (string)
  (cl:format cl:t "~A~%" string))

(defun array/hash-access (array/hash index)
  (cl:typecase array/hash
    (cl:hash-table (cl:gethash index array/hash))
    (cl:list (cl:elt array/hash index))))
