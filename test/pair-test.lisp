;;;;; Test suite for RUTILS PAIR
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest ht->pairs ()
  (should be eql ()
          (ht->pairs #h()))
  (should be equalp (list (pair :l :r) (pair "l" "r"))
          (ht->pairs #h(equal :l :r "l" "r"))))

(deftest pairs->ht ()
  (should be equalp #h()
          (pairs->ht ()))
  (should be equalp #h(equal :l :r "l" "r")
          (pairs->ht (list (pair :l :r) (pair "l" "r")) :test 'equal)))

(deftest setfable/lt ()
  (should be eq t
          (handler-case (progn (setf (rutil:lt (rutil:pair 0 0)) 1) t)
            (error (e) (declare (ignore e)) nil))))

(deftest setfable/rt ()
  (should be eq t
          (handler-case (progn (setf (rutil:rt (rutil:pair 0 0)) 1) t)
            (error (e) (declare (ignore e)) nil))))
