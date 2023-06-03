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

(cl:defmacro |if| (test then else)
  (cl:let ((evaled-test (cl:gensym "EVALED-TEST")))
    `(cl:let ((,evaled-test ,test))
       (cl:if (cl:or (cl:null ,evaled-test)
                     (cl:eq ,evaled-test |false|))
              ,else
              ,then))))

(cl:defparameter |true| '|true|)
(cl:defparameter |false| '|false|)

(cl:setf (cl:symbol-function '-) #'cl:-)
(cl:setf (cl:symbol-function '*) #'cl:*)
(cl:setf (cl:symbol-function '/) #'cl:/)

(cl:defun ! (thing)
  (cl:if (cl:and thing
                 (cl:not (cl:eq thing |false|)))
         '|false|
         '|true|))

(cl:defun == (left right)
  (cl:if (cl:equal left right)
         '|true|
         '|false|))

(cl:defun != (left right)
  (cl:if (cl:equal left right)
         '|false|
         '|true|))

(cl:defun < (left right)
  (cl:if (cl:< left right) |true| |false|))

(cl:defun > (left right)
  (cl:if (cl:> left right) |true| |false|))

(cl:defun + (left right)
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

(cl:defun make-hash (cl:&rest params)
  (cl:loop
     with ht = (cl:make-hash-table :test 'cl:equal)
     for (key value) on params by #'cl:cddr
     do (cl:setf (cl:gethash key ht) value)
     finally (cl:return ht)))

(cl:defun array/hash-access (array/hash index)
  (cl:typecase array/hash
    (cl:hash-table (cl:gethash index array/hash))
    (cl:list (cl:elt array/hash index))))
