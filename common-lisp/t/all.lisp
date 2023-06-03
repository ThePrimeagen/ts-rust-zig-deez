(defpackage #:deez/test/all
  (:use #:cl #:fiveam)
  (:export
   #:run-tests))
(in-package #:deez/test/all)

(def-suite deez
  :description "Parent suite for all deez tests.")

(defun run-tests ()
  (5am:run! 'deez))
