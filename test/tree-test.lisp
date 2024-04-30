;;;;; Test suite for RUTILS TREE
;;;;; see LICENSE file for permissions


(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)


(deftest dotree ()
  ;; 0th subtree (1 (2 3) (4 5)) first => 1
  ;; 1st subtree 1               first => 1
  ;; 2nd subtree (2 3)           first => 2
  ;; 3rd subtree 2               first => 2
  ;; 4th subtree 3               first => 3
  ;; 5th subtree (4 5)           first => 4
  ;; 6th subtree 4               first => 4
  ;; 7th subtree 5               first => 5
  (should be equal '(5 4 4 3 2 2 1 1)
          (let (rez)
            (dotree (el '(1 (2 3) (4 5)) rez)
              (push (first (mklist el)) rez)))))

(deftest doleaves ()
  (should be equal '(5 3)
          (let (rez)
            (doleaves (el '(1 (2 3) (4 5)) rez)
              (push el rez)))))

(deftest maptree ()
  (should be equal '(2 (3 4) (5 6))
          (maptree #'1+ '(1 (2 3) (4 5)))))

(deftest mapleaves ()
  (should be equal '(2 (3 4) (5 6))
          (mapleaves #'1+ '(1 (2 3) (4 5)))))

(deftest tree-size ()
  (should be = 0
          (tree-size '()))
  (should be = 2
          (tree-size '(:foo)))
  (should be = 3
          (tree-size '(:foo :bar)))
  (should be = 6
          (tree-size '(:foo (:bar :baz :foo))))
  (should be = 7
          (tree-size '(:foo (:bar :baz :foo) :bar))))

(deftest tree-depth ()
  (should be = 0
          (tree-depth ()))
  (should be = 1
          (tree-depth '(:foo)))
  (should be = 2
          (tree-depth '(:foo :bar)))
  (should be = 3
          (tree-depth '(:foo (:bar :baz :foo))))
  (should be = 3
          (tree-depth '(:foo (:bar :baz :foo) :bar))))
