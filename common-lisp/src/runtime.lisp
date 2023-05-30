(cl:defpackage #:deez/runtime
  (:export
   #:|let|
   #:|if|
   #:|else|
   #:|return|
   #:|fn|
   #:|true|
   #:|false|
   #:|==|
   #:|!=|
   #:|len|
   #:|push|
   #:|first|
   #:|rest|
   #:|puts|))
(cl:in-package #:deez/runtime)

(cl:defparameter |true| '|true|)
(cl:defparameter |false| '|false|)

(cl:defparameter |len| #'cl:length)
(cl:defparameter |push| (cl:lambda (array elem)
                          (cl:vector-push-extend elem array)
                          array))
(cl:defparameter |first| (cl:lambda (array)
                           (cl:if (cl:= (cl:length array) 0)
                                  #()
                                  (cl:aref array 0))))
(cl:defparameter |rest| (cl:lambda (array)
                          (cl:if (cl:< (cl:length array) 2)
                                 #()
                                 (cl:subseq array 1))))
(cl:defparameter |puts| (cl:lambda (string)
                          (cl:format cl:t "~A~%" string)))

(cl:defparameter array/hash-access (cl:lambda (array/hash index)
                                     (cl:typecase array/hash
                                       (cl:hash-table (cl:gethash index array/hash))
                                       (cl:list (cl:elt array/hash index)))))
