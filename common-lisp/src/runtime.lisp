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

(cl:defmacro defun (symbol lambda-list cl:&body body)
  `(cl:defparameter ,symbol (cl:lambda ,lambda-list
                              ,@body)))

(cl:defparameter |true| '|true|)
(cl:defparameter |false| '|false|)

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
