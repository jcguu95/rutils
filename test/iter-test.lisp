;;;;; Test suite for RUTILSX ITER
;;;;; see LICENSE file for permissions

;;; Test cases for Iterate.

;;; Copyright (c) 2003 Andreas Fuchs <asf@boinkor.net>
;;; Copyright (c) 2004-2007 Joerg Hoehle <hoehle@users.sourceforge.net>
;;; Copyright (c) 2024 Jin-Cheng Guu <jcguu95@gmail.com>

;;; License:
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(cl:in-package #:rutils.test)
(named-readtables:in-readtable rutils-readtable)

(deftest repeat ()
  ;; EDIT from
  ;; https://gitlab.common-lisp.net/iterate/iterate/-/blob/master/iterate-test.lisp
  (should be = 9
          (iter (:repeat 9) (:counting 1)))
  (should be = 3
          (iter (:repeat 2.5s0) (:counting t)))
  (should be = 0
          (iter (:repeat -1.5f0) (:counting t))))


(deftest locally. ()
  (should be equal '(1 0 2 0)
          (iter (:for i :in '(1 2 3)) (:repeat 2)
            (locally (:collect i) (:collect 0))))
  (should be equal '(1 2)
          (iter (:for i :in '(1 2 3)) (:repeat 2)
            (locally
                (declare (optimize safety))
              (declare (fixnum i)) (:collect i)))))

(deftest always ()
  (should be eq 2
          (iter (:repeat 3) (:always 2)))
  (should be eq t
          (iter (:repeat 0) (:always 2)))
  (should be eq t
          (iter (:for i :in '()) (:always i))))

(deftest always.finally ()
  ;; NOTE The original test seems wrong.
  (should be equal ""
          (with-output-to-string (*standard-output*)
            (iter (:repeat 1)
              ;; When ALWAYS fails, it RETURN-FROM's from the block and
              ;; thus skips FINALLY.
              (:always nil)
              (:finally (princ 42))))))

(deftest never.finally ()
  ;; NOTE The original test seems wrong.
  (should be equal ""
          (with-output-to-string (*standard-output*)
            (iter (:repeat 1)
              ;; See comment at ALWAYS.4
              (:never t)
              (:finally (princ 42))))))

(deftest thereis.finally ()
  (should be equal "42"
          (with-output-to-string (*standard-output*)
            (iter (:repeat 1)
              ;; See comment at ALWAYS.4
              (:thereis 43)
              (:finally (princ 42))))))

(deftest always.never.1 ()
  (should be eq 2
          (iter (:repeat 2)
            (:always 2)
            (:never nil)))
  (should be eq 2
          (iter (:for x :in '(b (2 . a)))
            (if (consp x) (:always (car x)) (:always x))
            (:never nil))))

(deftest thereis.finally ()
  (should be eq nil
          (iter (:repeat 3) (:thereis nil) (:finally (prin1 "hi"))))
  (should be equal "hi"
          (with-output-to-string (*standard-output*)
            (iter (:repeat 3) (:thereis nil) (:finally (princ "hi")))))
  (should be eq 2
          (iter (:repeat 3) (:thereis nil) (:finally (return 2)))))

(deftest thereis.finally-protected.1 ()
  (should be eq 4
          (iter (:repeat 3) (:thereis 4) (:finally-protected (prin1 "hi")))))

(deftest thereis.finally-protected.2 ()
  (should be equal "hi"
          (with-output-to-string (*standard-output*)
            (iter (:repeat 3) (:thereis 4) (:finally-protected (princ "hi"))))))

(deftest finding.such-that.2 ()
  (should be eq 2
          (iter (:for i :in '(7 -4 2 -3 4))
            (if (plusp i)
                (:finding    i  :such-that (evenp i))
                (:finding (- i) :such-that (oddp i))))))

(deftest finding.such-that.nest.1 ()
  (should be eq 2 ; not -1 as some old version did
          (iter (:for i :in '(1 2 3))
            (:finding (1+ i)
             :such-that #'(lambda (x)
                            (declare (ignore x))
                            (:collect (- i) into m))))))

;; ;; FIXME
;; (deftest finding.such-that.nest.2 ()
;;   (should be equal '(2 nil)             ; not -2 nil as some old version did
;;           (multiple-value-list
;;            (iter
;;              (:for i :in '(1 2 3))
;;              (:finding (1+ i)
;;               :such-that #'(lambda (x)
;;                              (:finding (- x)
;;                               :such-that #'(lambda (x) x nil)
;;                               :into n)
;;                              t)
;;               :into m)
;;              (:finally (return (values m n)))))))

(deftest finding.thereis.1 ()
  (should be eq 7
          (iter
            (:for x :in '(a 7 (-4 -3)))
            (:thereis (consp x))
            (:finding x :such-that (numberp x)))))

(deftest finding.thereis.2 ()
  (should be eq t
          (iter (:for x :in '(a (-4 -3) 7))
            (:thereis (consp x))
            (:finding x :such-that (numberp x)))))

(deftest finding.thereis.3 ()
  (should be eq nil
          (iter (:for x :in '(a #\b))
            (:thereis (consp x))
            (:finding x :such-that (numberp x)))))

(deftest finding.always.1 ()
  (should be eq t
          (iter (:for x :in '(-4 -2 -3))
            (:always (numberp x))
            (:finding x :such-that (plusp x) :on-failure t))))

(deftest finding.always.2 ()
  (should be eq 7
          (iter (:for x :in '(-4 7 -2 -3))
            (:always (numberp x))
            (:finding x :such-that (plusp x) :on-failure t))))

(deftest finding.always.3 ()
  (should be eq nil
          (iter (:for x :in '(-4 c -3))
            (:always (numberp x))
            (:finding x :such-that (plusp x) :on-failure t))))

(defun setup-hash-table (hash)
  (dotimes (i (random 100)) (declare (ignorable i))
        (setf (gethash (random 10000) hash) (random 10000))
        (setf (gethash (gensym) hash) (gensym))))

(deftest in-hashtable.keys ()
  (should be eq t
          (let* ((hash (make-hash-table))
                 (all-entries (progn (setup-hash-table hash) '()))
                 (generated-entries
                   (iter (:for (key) :in-hashtable hash)
                     (:collect key))))
            (maphash (lambda (key value) value (push key all-entries)) hash)
            (= (length all-entries)
               (length generated-entries)
               (length (union all-entries generated-entries
                              :test (hash-table-test hash)))))))

(deftest in-hashtable.items.1 ()
  (should be eq nil
          (let ((items nil)
                (hash (make-hash-table)))
            (setup-hash-table hash)
            (maphash (lambda (key item) key (push item items)) hash)
            (set-difference items
                            (iter (:for (key item) :in-hashtable hash)
                              ;; (declare (ignore key))
                              (:collect item))))))

(deftest in-hashtable.items.2 ()
  (should be eq nil
          (let ((items nil)
                (hash (make-hash-table)))
            (setup-hash-table hash)
            (maphash (lambda (key item) key (push item items)) hash)
            (set-difference items
                            (iter (:for (nil item) :in-hashtable hash)
                              (:collect item))))))

(deftest in-hashtable.1 ()
  (should be eq t
          (let* ((hash (make-hash-table))
                 (all-entries (progn (setup-hash-table hash) '()))
                 (generated-entries
                   (iter (as (key item) :in-hashtable hash)
                     (:collect (cons key item)))))
            (maphash #'(lambda (key value) (push (cons key value) all-entries)) hash)
            (= (length all-entries)
               (length generated-entries)
               (length (union all-entries generated-entries
                              :key #'car :test (hash-table-test hash)))))))

(deftest in-hashtable.destructuring.1 ()
  (should be equal '((1 . b) (6 . 3))
          (let ((hash (make-hash-table :test #'equal))
                (entries '(((a . b) . (1 . 2)) (("c" . 3) . (6 . "7")))))
            (iter
              (:for (k . v) :in entries)
              (setf (gethash k hash) v))
            (sort
             (iter
               (:for ((nil . k2) (v1 . v2)) :in-hashtable hash)
               (declare (ignorable v2))
               (:always (numberp v1))
               (:while k2)
               (:collect (cons v1 k2) :into vals)
               (:finally (return vals)))
             #'< :key #'car))))

;; FIXME error: #<UNDEFINED-FUNCTION SYM {70096F1A23}>
;; It's likely that iter expands the form wrongly.
;;
;; (deftest in-package.internals ()
;;   (should be equal '(() ())
;;           (let ((syms nil)
;;                 (iter-syms (iter (:for sym :in-package *package* :external-only nil)
;;                              (:collect sym))))
;;             (do-symbols (sym *package* nil)
;;               (push sym syms))
;;             (list
;;              (set-difference syms iter-syms :test #'eq)
;;              (set-difference iter-syms syms)))))

;; FIXME error: #<UNDEFINED-FUNCTION SYM {70096F1A23}>
;; It's likely that iter expands the form wrongly.
;;
;; (deftest in-package.externals.1 ()
;;   (should be equal '(() ())
;;           (let ((syms nil)
;;                 (iter-syms
;;                   (iter
;;                     (:for sym :in-package '#:cl-user external-only t)
;;                     (:collect sym))))
;;             (do-external-symbols (sym '#:cl-user nil)
;;               (push sym syms))
;;             (list
;;              (set-difference syms iter-syms :test #'eq)
;;              (set-difference iter-syms syms)))))

(deftest in-package.externals.2 ()
  (should be eq t
          (let ((sym-count 0))
            (do-external-symbols (sym '#:iter) (declare (ignore sym))
              (incf sym-count))
            (= sym-count
               (iter (:for () :in-package '#:iter external-only t)
                 (:count 1))))))

;; FIXME
;; (deftest in-package.generator ()
;;   (should be equal (() ())
;;           (let ((syms nil)
;;                 (iter-syms (iter (:generate sym :in-package *package*)
;;                              (:collect (:next sym)))))
;;             (do-symbols (sym *package*)
;;               (push sym syms))
;;             (list
;;              (set-difference syms iter-syms :test #'eq)
;;              (set-difference iter-syms syms)))))

(deftest in-packages.external ()
  (should be equal (() ())
          (let ((syms nil)
                (iter-syms (iter (:as (sym access package) :in-packages '(#:cl-user)
                                  :having-access (:external))
                             (:collect sym))))
            (do-external-symbols (sym '#:cl-user nil)
              (push sym syms))
            (list
             (set-difference syms iter-syms :test #'eq)
             (set-difference iter-syms syms)))))

;; FIXME
;; (deftest in-packages.generator-access ()
;;   (should be eq t
;;           (let ((iter-syms
;;                   (iter
;;                     (:generate (sym access)
;;                      :in-packages (list (find-package "COMMON-LISP")))
;;                     (:repeat 1)
;;                     (:next sym)
;;                     (:collect (list sym access)))))
;;             (equal (multiple-value-list
;;                     (find-symbol (symbol-name (caar iter-syms)) "COMMON-LISP"))
;;                    (car iter-syms)))))

(deftest in-stream.1 ()
  (should be equal '(10 () 2)
          (iter (:as x :in-stream (make-string-input-stream "#xa()2"))
            (:collect x))))

;; FIXME
;; (deftest in-stream.previous ()
;;   (should be equal '(1 10 ())
;;           (iter
;;             (:for x :in-stream (make-string-input-stream "#xa()2"))
;;             (:as p :previous x :initially 1)
;;             (:collect p))))

(deftest in-stream.2 ()
  (should be eq nil
          ;; This fails :in cmucl, sbcl and gcl, because string-input-streams
          ;; are :always open, even after close.
          (let ((s (make-string-input-stream "(")))
            (ignore-errors
             (iter (:for x :in-stream s :using #'read)))
            (open-stream-p s))))

(deftest in-stream.3 ()
  (should be eq 10
          (iter (:for c :in-stream (make-string-input-stream "235")
                 :using #'read-char)
            (:accumulating (digit-char-p c) :by #'+ :initial-value 0))))

(deftest in-stream.reducing ()
  (should be eq 13
          (iter (:with s := (make-string-input-stream "(+ 2)(+ 10)(- 5)(+ 6)"))
            (:for (op num) :in-stream s)
            (:reducing num :by op :initial-value 0))))

(deftest in-stream.accumulating ()
  (should be eq -1 ; (6 + (5 - (10 + (2 + 0))))
          (iter (:with s := (make-string-input-stream "(+ 2)(+ 10)(- 5)(+ 6)"))
            (:for (op num) :in-stream s)
            (:accumulating num :by op :initial-value 0))))

;; FIXME
;; (deftest in-stream.generate ()
;;   (should be eq 13
;;           (iter (:with s = (make-string-input-stream "2 + 10 - 5 + 6"))
;;             (:with start = (read s))
;;             (:generate e :in-stream s :using #'read)
;;             (:as op = (next e))
;;             (:for arg = (next e))
;;             (:reducing arg :by op :initial-value start))))

(deftest in-stream.reiter ()
  (should be equal '(3 4)
          (let ((s (make-string-input-stream "1 2 3 4 5")))
            (iter (:for n :below 2)
              (:for line :in-stream s))
            (iter (:for n :below 2)
              (:for line :in-stream s)
              (:collect line)))))

(deftest reducing.0 ()
  (should be eq 13
          (iter (:with expr = '(2 + 10 - 5 + 6))
            (:with start = (pop expr))
            (:for (op arg) :on expr :by #'cddr)
            (:reducing arg :by op :initial-value start))))

(deftest until.1 ()
  (should be equal '(10 30)
          (multiple-value-list
           (iter (:with rest = 235) (:with digit = 0)
             (multiple-value-setq (rest digit) (floor rest 10))
             (:sum digit :into sum)
             (:multiplying digit :into product)
             (:until (zerop rest))
             (:finally (return (values :sum product)))))))

;; FIXME
;; (deftest until.2 ()
;;   (should be eq 2
;;           (iter
;;             (:for i :in-sequence '#(1 2 -3 6))
;;             (:until (zerop (:sum i :into x)))
;;             (:multiplying i))))

;; FIXME
;; (deftest while.1 ()
;;   (should be equal '(1 2)
;;           (iter (:for i :in-sequence '#(1 2 -3 6))
;;             (:while (< (length (:collect i)) 2)))))

(deftest else.1 ()
  (should be = 1
          (iter (:repeat 0)
            (:else (return 1)))))

(deftest else.2 ()
  (should be = 2
          (iter (:for i :below -3)
            (:else (return 2)))))

;; tests :for my examples:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *an-alist* '((a . 2) (b . 3) (zero . 10) (c . 4) (one . 20) (d . 5) (e . 99)))
  (defparameter *list-of-lists* (loop :for i from 0 to 100
                                      :collect (loop :for len from 0 to (random 20)
                                                    :collect len)))
  (defun longest-list (list1 list2)
    (if (< (length list2) (length list1))
        list1
        list2)))

(deftest collect.1 ()
  (should be equal
          (multiple-value-list
           (iter (:as (key . item) :in *an-alist*)
             (:collect key :into keys)
             (:collect item :into items)
             (:finally (return (values keys items)))))
          '(#.(loop :for (key . nil) :in *an-alist*
                    :collect key)
            #.(loop :for (key . item) :in *an-alist*
                    :collect item))))

;; (deftest generate.1 ()
;;   (should be equal
;;           (iter (:generate i from 0 to 6)
;;             (:for (key . value) :in *an-alist*)
;;             (when (>= value 10)
;;               (:collect (cons key (next i)))))
;;           #.(loop :with counter = 0
;;                   :for (key . value) :in *an-alist*
;;                   :when (>= value 10)
;;                     :collect (cons key (prog1 counter (incf counter))))))

;; FIXME :finding
;; (deftest find-longest-list.1 ()
;;   (should be equal #.(reduce #'longest-list *list-of-lists*)
;;           (iter (:for elt :in *list-of-lists*)
;;             (:finding elt :maximizing (length elt)))))

;; FIXME :finding
;; (deftest find-longest-list.2 ()
;;   (should be equal
;;           (iter
;;             (:for elt :in *list-of-lists*)
;;             (:finding elt :maximizing (length elt) :into (e m))
;;             (:finally (return m)))
;;           #.(reduce #'max *list-of-lists* :key #'length)))

;; FIXME :finding
;; (deftest find-longest-list.3 ()
;;   (should be equal
;;           (iter
;;             (:for elt :in *list-of-lists*)
;;             (:finding elt :maximizing #'length))
;;           #.(reduce #'longest-list *list-of-lists*)))

;; FIXME :finding
;; (deftest find-longest-list.4 ()
;;   (should be equal
;;           (iter
;;             (:for elt :in *list-of-lists*)
;;             (:finding elt :maximizing #'length :into (e m))
;;             (:finally (return m)))
;;           #.(reduce #'max *list-of-lists* :key #'length)))

(deftest maximize.1 ()
  (should be equal
          (iter
            (:for elt :in *list-of-lists*)
            (:maximizing (length elt) :into m)
            (:finally (return m)))
          #.(reduce #'max *list-of-lists* :key #'length)))

(deftest maximize.2 ()
  (should be equal
          (iter
            (:for elt :in *list-of-lists*)
            (:maximize (length elt)))
          #.(reduce #'max *list-of-lists* :key #'length)))

;; FIXME
;; (deftest finding.minimizing.1 ()
;;   (should be equal
;;           (iter
;;             (:for elt :in *list-of-lists*)
;;             (:finding elt :minimizing #'length :into (e m))
;;             (:finally (return m)))
;;           #.(reduce #'min *list-of-lists* :key #'length)))

(deftest minimize.1 ()
  (should be equal
          (iter
            (:for elt :in *list-of-lists*)
            (:minimizing (length elt) :into m)
            (:finally (return m)))
          #.(reduce #'min *list-of-lists* :key #'length)))

(deftest minimize.2 ()
  (should be equal
          (iter
            (:for elt :in *list-of-lists*)
            (:minimize (length elt)))
          #.(reduce #'min *list-of-lists* :key #'length)))

(deftest subblocks.maximize.1 ()
  (should be equal
          (iter outer (:for elt :in *list-of-lists*)
            (iter running (:for e :in elt)
              (:in outer (:maximize e)))
            (:maximizing (length elt)))
          #.(reduce #'max *list-of-lists* :key #'length)))

(deftest subblocks.minimize.1 ()
  (should be eq 0
          (iter outer
            (:for elt :in *list-of-lists*)
            (:minimizing (length elt))
            (iter running
              (:for e :in elt)
              (:in outer (:minimize e))))))

;; FIXME
;; (deftest maximize.3 ()
;;   (should be eql -3
;;           (iter
;;             (:for i :in-vector '#(-3))
;;             (:maximize i))))

;; (deftest minimize.3 ()
;;   (should be eql 3
;;           (iter
;;             (:as i :in-vector '#(3))
;;             (:minimize i))))

(deftest maximize.multiple ()
  (should be eql 3
          (iter
            (:as i :from 3 :downto -3 :by 2)
            (:maximize i)
            (:for j :from -1)
            (:maximizing j))))

(deftest minimize.multiple ()
  (should be eql -4
          (iter
            (:as i :from -3 to 3 :by 2)
            (:minimize i :into x)
            (:for j :downfrom -1)
            (:minimizing j :into x)
            (:finally (return x)))))

;; FIXME 1 2 3 4
;; (deftest accumulate.1 ()
;;   (should be eql 30
;;           (iter
;;             (:for c :in-string "235")
;;             (declare (type character c))
;;             (:accumulate (digit-char-p c) :by '* :initial-value 1))))

;; (deftest accumulate.2 ()
;;   (should be eql 30
;;           (iter
;;             (:for c :in-sequence "235")
;;             (:accumulating (digit-char-p c) :by #'* :initial-value 1))))

;; (deftest accumulate.3 ()
;;   (should be equal '(5 3 2 . 1)
;;           (iter
;;             (:for c :in-sequence "235")
;;             (:accumulate (digit-char-p c) :by 'cons :initial-value 1))))

;; (deftest accumulate.4 ()
;;   (should be equal '(5 3 2)
;;           (iter
;;             (:for c :in-vector "235")
;;             (:accumulating (digit-char-p c) :by #'cons))))

(deftest accumulate.5 ()
  (should be eq nil
          (iter
            (:repeat 0)
            (:accumulating 1 :by #'cons))))

(deftest accumulate.6 ()
  (should be eq 2
          (iter
            (:repeat 0)
            (:accumulate 1 :by #'cons :initial-value 2))))

;; FIXME Type Warning
;; (deftest in-string.downto.1 ()
;;   (should be equal '(3 5)
;;           (iter
;;             (:for c :in-string "235" :downto 1)
;;             (:accumulate (digit-char-p c) :by 'cons))))

;; FIXME Type Warning
;; (deftest in-sequence.downto.1 ()
;;   (should be equal '(3 5)
;;           (iter
;;             (:for c :in-sequence "235" :downto 1)
;;             (:accumulate (digit-char-p c) :by #'cons))))

;; FIXME
;; (deftest reducing.1 ()
;;   (should be equal '(((1 2) 3) 5)
;;           (iter
;;             (:for c :in-string "235")
;;             (:reducing (digit-char-p c) :by 'list :initial-value 1))))

;; ;; FIXME
;; (deftest reducing.2 ()
;;   (should be equal '(((-1 0) 1) 2)
;;           (iter
;;             (:as x index-of-string "235")
;;             (:reducing x :by #'list :initial-value -1))))

(deftest reducing.3 ()
  (should be eql -1
          (iter
            (:repeat 0)
            (:reducing 1 :by 'cons :initial-value -1))))

(deftest reducing.4 ()
  (should be eql -12
          (iter
            (:for i :from 3 :to 5)
            (:reducing i :by #'- :initial-value '0))))

;; FIXME
;; (deftest reducing.5 ()
;;   (should be equal '(3 . 3)
;;           (iter
;;             (:for x :in-vector #(3))
;;             (:reducing (cons x x) :by #'list))))

;; FIXME
;; (deftest reducing.6 ()
;;   (should be equal '(nil (3 . 3))
;;           (iter
;;             (:for x :in-vector (vector 3))
;;             (:reducing (cons x x) :by #'list :initial-value nil))))

;; synonyms (e.g. GENERATING, COLLECTING) didn't work

;; (deftest generate.destructuring.1 ()
;;   (should be equal '(a 2 c)
;;           (iter
;;             (:generate (key . item) :in '((a . 1) (b . 2) (c .3)))
;;             (:collect (:next key))
;;             (:collect (:next item)))))

;; (deftest generating.destructuring.1 ()
;;   (should be equal '(a 2 c)
;;           (iter
;;             (:generating (key . item) :in '((a . 1) (b . 2) (c .3)))
;;             (:collect (:next key))
;;             (:collect (:next item)))))

;; (deftest for.generate-t.destructuring.1 ()
;;   (should be equal '(a 2 c)
;;           (iter
;;             (:for (key . item) :in '((a . 1) (b . 2) (c .3)) :generate t)
;;             (:collect (:next key))
;;             (:collect (:next item)))))

;; (deftest generate.next.1 ()
;;   (should be equal '(a b d g h h i i k l)
;;           (iter
;;             (:generate c :in '(a b c d e f g h i j k l m n o p q))
;;             (:for s :in '(1 1 2 3 1 0 1 0 2 1))
;;             (:collect (:next c s)))))

;; (deftest generate.previous.1 ()
;;   (should be equal '((nil a) (a b) (b d) (d g) (g h) (h h) (h i) (i i) (i k) (k l))
;;           (iter
;;             (:generate c :in '(a b c d e f g h i j k l m n o p q))
;;             (:for s :in '(1 1 2 3 1 0 1 0 2 1))
;;             (:for x = (:next c s))
;;             (:as y :previous x)
;;             (:collect (list y x)))))

;; (deftest generate.next.2 ()
;;   (should be equal "24"
;;           (with-output-to-string (*standard-output*)
;;             (iter
;;               (:generate i :in '(1 2 3 4 5))
;;               (princ (:next i 2))))))

;; (deftest if.1 ()
;;   (should be equal '(0 3)
;;           (iter
;;             (:generate x :in-vector '#(t nil nil t))
;;             (:as i :from 0)
;;             (if (:next x) (:collect i)))))

;; FIXME :generate
;; (deftest if.2 ()
;;   (should be equal '(0 3)
;;           (iter (:generate x :in-vector '#(t nil nil t) :with-index i)
;;             (if (:next x) (:collect i)))))

;; FIXME :generate
;; (deftest or.1 ()
;;   (should be equal '(a 2 #\c 1)
;;           (iter
;;             (:generate x :in '(a nil nil 1))
;;             (:generate y :in-vector '#(2 #\c #\d))
;;             (:collect (or (:next x) (:next y))))))

;; FIXME :generate
;; (deftest or.2 ()
;;   (should be equal '(a 2 3 1 #\c)
;;           (iter
;;             (:generate x :in '(a nil nil 1 nil))
;;             (:generate y :in-sequence '#(2 nil #\c #\d))
;;             (:collect (or (:next x) (:next y) 3)))))

;; FIXME :generate
;; (deftest setf.1 ()
;;   (should be equal #(0 1 2 3)
;;           (iter
;;             (:generate i :from 0 :to 3)
;;             (:with v := (vector 'a 'b 'c 'd))
;;             (setf (aref v (:next i)) i)
;;             (:finally (return v)))))

;; FIXME :generate
;; (deftest setf.2 ()
;;     ;; These setf tests fail :in CormanLisp 2.0 because ccl does
;;     ;; not respect setf evaluati:on order rules.
;;   (should be equal #(1 b 3 d)
;;           (iter
;;             (:generate i :from 0 :to 3)
;;             (:with v := (vector 'a 'b 'c 'd))
;;             (setf (aref v (:next i)) (:next i))
;;             (:finally (return v)))))

;; FIXME :generate
;; (deftest setf.3 ()
;;   (should be equal #(2 b c 5)
;;           (iter
;;             (:generate i :in '(0 1 2 3 4 5))
;;             (:with v := (vector 'a 'b 'c 'd))
;;             (setf (aref v (:next i)) (:next i 2))
;;             (:finally (return v)))))

;; (deftest setf.4 ()
;;   (should be equal #(1 b 3 d)
;;           (iter
;;             (:generate i :from 0 :to 3)
;;             (:with v := (vector 'a 'b 'c 'd))
;;             (setf (apply #'aref v (list (:next i))) (:next i))
;;             (:finally (return v)))))

;; (deftest after-each.1 ()
;;   (should be equal '(a 0 b 0 c 0)
;;           (iter
;;             (:after-each (:collecting 0))
;;             (:generate i :in '(a b c))
;;             (:adjoining (:next i)))))

(deftest after-each.2 ()
  (should be equal '(0 1 2 3)
          (iter
            (:with i := 0)
            (:while (< i 4))
            (:after-each (incf i))      ; the C programmer's :for (;;) loop
            (:collect i))))

(deftest after-each.3 ()
  (should be equal '(0 1 2 3)
          (iter
            (:with i := 0)
            (:while (< i 4))
            (:collect i)
            (:after-each (incf i)))))

(deftest next-iteration.1 ()
  (should be eq 5
          (iter
            (:for i :below 10)
            (when (oddp i) (:next-iteration))
            (:count t))))

(deftest next-iteration.2 ()
  (should be equal
          (iter (:for thing :in '(var &optional :else &key (test #'eql)))
            (:collect
                (cond ((consp thing) (first thing))
                      ((not (member thing lambda-list-keywords)) thing)
                      (t (next-iteration)))))
          '(var else test)))

;;;; tests :from the documentation:

(deftest collect.2 ()
  (should be equal
          (iter (:for i :from 1 :to 10)
            (:collect i))
          '(1 2 3 4 5 6 7 8 9 10)))

(deftest for-in.2 ()
  (should be equal
          (iter (:for el :in '(1 2 3 4 5 6 f 7 8 9 a 10))
            (if (and (numberp el) (oddp el))
                (:collect el)))
          '(1 3 5 7 9)))

(deftest for.destructuring.1 ()
  (should be equal
          (iter
            (:for (key . item) :in '((a . 10) (b . 20) (c . 30)))
            (declare (ignorable item))
            (:for i :from 0)
            (declare (fixnum i))
            (:collect (cons i key)))
          '((0 . a) (1 . b) (2 . c))))

(deftest repeat.0 ()
  (should be equal
          (with-output-to-string (*standard-output*)
            (iter (:repeat 100)
              (print "I will not talk :in class.")))
          #.(with-output-to-string (*standard-output*)
              (dotimes (i 100)
                (declare (ignorable i)) ; cmucl/sbcl compla:in about (ignore i)
                (print "I will not talk :in class.")))))

;;; for.next.1 and for.do-next.1 used :to be broken :in older versions;
;;; they didn't WALK their :NEXT args.
(deftest for.next.1 ()
  (should be eq
          (iter
            (:initially (setq i 0))
            (:for i :next (if (> i 10) (:terminate) (1+ i)))
            (:finally (return i)))
          11))

;;; This gave STYLE-WARNINGS :for undefined i :in old versions.
(deftest for.do-next.1 ()
  (should be eq
          (iter (:initially (setq i 0))
            (:as i :do-next (if (> i 10) (:terminate) (incf i)))
            (:finally (return i)))
          11))

(deftest for.do-next.2 ()
  (should be eq
          ;; :INITIALLY not needed because 0 is inferred :from type declaration
          (iter
            (:for i :do-next (if (> i 7) (:terminate) (incf i)))
            (declare (type fixnum i))
            (:finally (return i)))
          8))

(deftest for.do-next.3 ()
  (should be equal
          (iter (:for a :from 1 :to 3)
            (:for b := (1+ (* a a)))
            ;; (values ...) is supported, even though (x y) would do
            (:for (values x y) :do-next (dsetq (values x y) (floor b a)))
            (:collect x) (:collect y))
          '(2 0 2 1 3 1)))

(deftest for.next.walk ()
  (should be equal
          (iter
            (:repeat 2)
            (:for x :next (progn (:after-each (:collect 1)) 2))
            (:collect x))
          '(2 1 2 1)))

(deftest for.do-next.walk ()
  (should be equal
          (iter
            (:repeat 2)
            (:for x :do-next (progn (:after-each (:collect 1)) (dsetq x 2)))
            (:collect x))
          '(2 1 2 1)))

;; FIXME Unknown keyword :previous
;;
;; (deftest for.next.previous ()
;;   (should be equal
;;           (iter
;;             (:for i :from 2 :to 4)
;;             (:for x :next (progn (:after-each (:collect i)) (- i)))
;;             (:for z :previous x :initially 0)
;;             (:nconcing (list z x)))
;;           '(0 -2 2 -2 -3 3 -3 -4 4)))

;; FIXME Unknown keyword :previous
;; (deftest for.do-next.previous ()
;;   (should be equal
;;           (iter
;;             (:for i :from 2 :to 4)
;;             (:for x :do-next (progn (setq x (- i)) (:after-each (:collect i))))
;;             (:for z :previous x :initially 0)
;;             (:appending (list z x)))
;;           (0 -2 2 -2 -3 3 -3 -4 4)))

;; (deftest for-nongenerator.1 ()
;;   (should be equal
;;           (iter
;;             (:for el :in '(a b c d))
;;             (:generate i up:from 1)
;;             (if el (:collect (cons el (:next i)))))
;;           #.(iter
;;               (:for el :in '(a b c d))
;;               (:for i up:from 1)
;;               (if el (:collect (cons el i))))))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.in ()
;;   (should be equal
;;           (iter
;;             (:for el :in '(1 2 3 4))
;;             (:for pp-el :previous el :back 2 :initially 0)
;;             (:collect pp-el))
;;           '(0 0 1 2)))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.type.1 ()
;;   (should be equal
;;           (iter (:for el :in '(1 2 3 4))
;;             (declare (type integer el))
;;             (:for pp-el :previous el :back 2)
;;             (:collect pp-el))
;;           '(0 0 1 2)))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.index-of-string.1 ()
;;   (should be equal
;;           (iter
;;             (:as x index-of-string "235")
;;             (:as p :previous x :initially 9)
;;             (:collecting p))
;;           '(9 0 1)))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.in-string.with-index ()
;;   (should be equal
;;           (iter
;;             (:as x :in-string "235" :with-index y)
;;             (:as p :previous y :initially 9)
;;             (:collecting p))
;;          '(9 0 1)))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.index-of-vector ()
;;   (should be equal
;;           (iter
;;             (:as x :index-of-vector '#(2 3 4 5))
;;             (:as p :previous x :initially 9 :back 2)
;;             (:collecting p))
;;           '(9 9 0 1)))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.in-vector.with-index ()
;;   (should be equal
;;           (iter
;;             (:as x :in-vector '#(2 3 4 5) :with-index y)
;;             (:as p :previous y :initially 9 :back 2)
;;             (:collecting p))
;;           (9 9 0 1)))

;; FIXME Unknown keyword :previous
;; (deftest for.previous.var-with-type-declaration ()
;;   (should be equal
;;           (iter
;;             (:for i :from 1 :to 5)
;;             (:for (the fixnum i-prev) :previous i)
;;             (:collect i-prev))
;;           '(nil 1 2 3 4)))

(deftest for.first.1 ()
  (should be equal
          (iter
            (:for num :in '(20 19 18 17 16))
            (:for i :first num :then (1+ i))
            (:collect i))
          '(20 21 22 23 24)))

(deftest for.initially.1 ()
  (should be equal
          (iter
            (:with (v z))
            (:for i :initially (length v) :then (1+ i))
            (:collect (cons i z))
            (:while (evenp i)))
          '((0) (1))))

(deftest sum.1 ()
  (should be eq
          (iter
            (:for el :in '(100 200 300))
            (:sum el :into x)
            (declare (fixnum x))
            (:finally (return x)))
          600))

(deftest collect.3 ()
  (should be equal
          (iter
            (:for i :from 1 :to 5)
            (:collect i))
          '(1 2 3 4 5)))

(deftest collect.4 ()
  (should be equal
          (iter
            (:for i :from 1 :to 5)
            (:collect i :at :beginning))
          '(5 4 3 2 1)))

(deftest collect.5 ()
  (should be equal
          (iter
            (:for i :from 1 :to 4)
            (:collect i :at :end))
          '(1 2 3 4)))

(deftest collect.6 ()
  (should be equal
          (iter
            (:for i :from 1 :to 3)
            (:collect i :at :start))
          '(3 2 1)))

(deftest collect-by.1 ()
  (should be equal
          (iter
            (:for i :downfrom 10 :by 2)
            (:repeat 3)
            (:collect i))
          '(10 8 6)))

;; (deftest in-vector.by.1 ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector '#(0 1 2 3 4) :by 2)
;;             (:collect i))
;;           '(0 2 4)))

(deftest index-of-vector.by.1 ()
  (should be equal
          (iter
            (:for i :index-of-vector '#(0 1 2 3 4) :by 2)
            (:collect i))
          '(0 2 4)))

;; (deftest in-vector.downto.1 ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector '#(0 1 2 3 4) :downto 0)
;;             (:collect i))
;;           '(4 3 2 1 0)))

;; FIXME
;; caught WARNING:
;;   Constant NIL conflicts with its asserted type NUMBER.
;;   See also:
;;     The SBCL Manual, Node "Handling of Types"
;;
;; (deftest index-of-vector.downto.1 ()
;;   (should be equal
;;           (iter
;;             (:for i :index-of-vector #(0 1 2 3 4) :downto 0)
;;             (:collect i))
;;           '(4 3 2 1 0)))

;; (deftest in-vector.downto.2 ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector '#(0 1 2 3 4) :downto 0 :by 2)
;;             (:collect i))
;;           '(4 2 0))) ; erroneously got (3 1) :in some past

(deftest index-of-vector.downto.2 ()
  (should be equal
          (iter
            (:for i :index-of-vector #(0 1 2 3 4) :downto 0 :by 2)
            (:collect i))
          '(4 2 0)))

;; (deftest generate.in-vector.downto.1 ()
;;   (should be equal
;;           (iter
;;             (:generate i :in-vector #(0 1 2 3 4) :downto 0 :by 2)
;;             (:collect (:next i)))
;;           '(4 2 0)))

;; (deftest generate.index-of-vector.downto.1 ()
;;   (should be equal
;;           (iter
;;             (:generate i :index-of-vector '#(0 1 2 3 4) :downto 0 :by 2)
;;             (:collect (:next i)))
;;           (4 2 0)))

;; FIXME missing clause :if-first-time
;; (deftest if-first-time.1 ()
;;   (should be equal
;;           (with-output-to-string (*standard-output*)
;;             (iter
;;               (:for i :from 200 :to 203)
;;               (:if-first-time (format t "honka"))))
;;           "honka"))

;; FIXME missing clause :if-first-time
;; (deftest if-first-time.2 ()
;;   (should be equal
;;           (with-output-to-string (*standard-output*)
;;             (iter (:for i :from 200 :to 204)
;;               (if (oddp i) (:if-first-time (princ "honka") (princ "tah")))))
;;           "honkatah"))

;; FIXME missing clause :if-first-time
;; (deftest if-first-time.3 ()
;;   (should be equal
;;           (iter
;;             (:for i :to 5)
;;             (when (oddp i)
;;               (:if-first-time nil (:collect -1))
;;               (:collect i)))
;;           '(1 -1 3 -1 5)))

(deftest first-time-p.0 ()
  (should be equal
          (with-output-to-string (*standard-output*)
            (iter (:for el :in '(nil 1 2 nil 3))
              (when el
                (unless (first-time-p)
                  (princ ", "))
                (princ el))))
          "1, 2, 3"))

(deftest first-time-p.1 ()
  (should be equal
          (iter (:for i :to 5)
            (if (first-time-p) (:collect -1))
            (if (first-time-p) (:collect -2))
            (when (oddp i)
              (if (first-time-p) nil (:collect -1))
              (:collect i)))
          '(-1 -2 1 -1 3 -1 5)))

(deftest first-iteration-p.1 ()
  (should be equal
          (iter (:for i :to 5)
            (if (:first-iteration-p) (:collect -1))
            (if (:first-iteration-p) (:collect -2))
            (when (oddp i)
              (if (:first-iteration-p) nil (:collect -1))
              (:collect i)))
          '(-1 -2 -1 1 -1 3 -1 5)))

(deftest collect.multiple.1 ()
  (should be equal
          (iter
            (:for i :from 1 :to 10)
            (:collect i :into nums)
            (:collect (sqrt i) :into nums)
            (:finally (return nums)))
          '#.(loop :for i :from 1 :to 10
                   :collect i
                   :collect (sqrt i))))

;; (deftest collect.type.string.1 ()
;;   (should be equal
;;           (locally (declare (optimize safety (debug 2) (speed 0) (space 1)))
;;             (iter
;;               (declare (iter:declare-variables))
;;               (:for s :in-vector '#(\a |b| |cD|))
;;               (:collect (char (symbol-name s) 0) :result-type string)))
;;           "abc"))

;; FIXME Unknown keyword type :result-type
;; (deftest collect.type.string.2 ()
;;   (should be equal
;;           (iter
;;             (:for c
;;              :in-stream (make-string-input-stream "aBc")
;;              :using #'read-char)
;;             (when (digit-char-p c 16)
;;               (:collect c :result-type string)))
;;           "aBc"))

;; FIXME in-string is broken
;; (deftest collect.type.string.3 ()
;;   (should be equal "32"
;;           (iter
;;             (:for c :in-string "235" :downfrom 1)
;;             (:collect c :into s :result-type string)
;;             (:finally (return s)))))

;; (deftest collect.type.vector.1 ()
;;   (should be equal
;;           (locally (declare (optimize safety (debug 2) (speed 0) (space 1)))
;;             (iter (declare (iter:declare-variables))
;;               (:for s :in-vector '#(\a |b| |cD|))
;;               (:collect (char (symbol-name s) 0) :result-type vector)))
;;           #(#\a #\b #\c)))

;; (deftest collect.type.vector.2 ()
;;   (should be equal
;;           (iter
;;             (:for c :in-vector "235" :downfrom 1)
;;             (:collect (digit-char-p c) :into v :result-type vector)
;;             (:finally (return v)))
;;           #(3 2)))

(deftest adjoin.1 ()
  (should be equal
          (iter
            (:for i :in '("ab" "aB" "abc" "AB"))
            (:adjoining i :at :start :test #'string-equal))
          '("abc" "ab")))

(deftest adjoin.2 ()
  (should be equal
          (iter
            (:for i :in '("ab" "aB" "abc" "AB"))
            (:adjoining i :at :start))
          '("AB" "abc" "aB" "ab")))

(deftest adjoin.3 ()
  (should be equal
          (iter
            (:for i :in '("ab" "aB" "abc" "AB"))
            (:adjoining i :at end #:test #'string-equal))
          '("ab" "abc")))

(deftest adjoin.4 ()
  (should be equal
          (iter
            (:for i :in '("ab" "aB" "abc" "AB"))
            (:adjoining i :at :end))
          '("ab" "aB" "abc" "AB")))

(deftest adjoin.5 ()
  (should be equal
          (iter (:for i :in '("ab" "aB" "abc" "AB"))
            (:adjoining (string-downcase i) :at :start :test #'string-equal))
          '("abc" "ab")))

(deftest adjoin.6 ()
  (should be equal
          (iter
            (:for i :in '("ab" "aB" "abc" "AB"))
            (:adjoining (string-upcase i) #:at :end test #'string=))
          '("AB" "ABC")))

(deftest append.1 ()
  (should be equal
          (iter
            (:for l :on '(1 2 3))
            (:appending l :at :start))
          '(3 2 3 1 2 3)))

(deftest nconc.1 ()
  (should be equal
          (iter
            (:for l :on (list 1 2 3))
            (:nconcing (copy-list l) :at :beginning))
          '(3 2 3 1 2 3)))

(deftest append.2 ()
  (should be equal
          (iter
            (:for l :on '(1 2 3))
            (:appending l :at #:end))
          '(1 2 3 2 3 3)))

(deftest nconc.2 ()
  (should be equal
          (iter
            (:for l :on (list 1 2 3))
            (:nconcing (copy-list l) :at end))
          '(1 2 3 2 3 3)))

(deftest append.3 ()
  (should be equal
          (iter
            (:for l :on '(1 2 3))
            (:appending l :into x) (:finally (return x)))
          '(1 2 3 2 3 3)))

(deftest nconc.3 ()
  (should be equal
          (iter
            (:for l :on (list 1 2 3))
            (:nconcing (copy-list l) :into x) (:finally (return x)))
          '(1 2 3 2 3 3)))

(deftest append.4 ()
  (should be eq
          (iter
            (:for l :on '(1 2 3))
            (:appending l :into x))
          nil))

(deftest nconc.4 ()
  (should be eq
          (iter
            (:for l :on (list 1 2 3))
            (:nconcing (copy-list l) :into x))
          nil))

(deftest append.5 ()
  (should be equal
          (iter
            (:for l :on '(1 2 3))
            (:appending l :at #:end)
            (:collect (first l)))
          '(1 2 3 1 2 3 2 3 3)))

(deftest append.6 ()
  (should be equal
          (iter
            (:for l :on '(1 2 3))
            (:appending l :at :end)
            (:collect l))
          '(1 2 3 (1 2 3) 2 3 (2 3) 3 (3))))

(deftest nconc.5 ()
  (should be equal
          (iter
            (:for l :on (list 1 2 3))
            (:collect (first l))
            (:nconcing (copy-list l) :at end))
          '(1 1 2 3 2 2 3 3 3)))

(deftest union.1 ()
  (should be equal
          (iter (:for l :on '(a b c))
            (:unioning l)
            (:collect (first l)))
          '(a b c a b c)))

(deftest union.2 ()
  (should be equal
          (iter
            (:for l :on '(a b c))
            (:collecting (first l))
            (:unioning l :test #'eql))
          '(a b c b c)))

;; (deftest union.3 ()
;;   (should be equal
;;           (iter (:for l :in-vector '#("a" "A" "aB" "ab" "AB"))
;;             (:unioning (list l) :test #'string-equal))
;;           '("a" "aB")))

;; (deftest nunion.3 ()
;;   (should be equal
;;           (iter
;;             (:for l :in-vector '#("a" "A" "aB" "ab" "AB"))
;;             (:nunioning (list l) :test #'string-equal :at :start))
;;           '("aB" "a")))

(deftest value.minimize ()
  (should be equal
          (iter
            (:for i :from 4 :downto -3 :by 3)
            (:collect (:minimize (* i i) :into foo)))
          '(16 1 1)))

(deftest value.maximize ()
  (should be equal
          (iter
            (:for i :from 3 :to 5)
            (:sum (:maximize (- i 2) :into foo)))
          6))

;; (deftest value.finding-maximizing.1 ()
;;   (should be equal
;;           (iter
;;             (:for i :from 3 :to 6)
;;             (:adjoining (:finding (* i i) :maximizing #'integer-length
;;                          :into foo)
;;              :test #'=))
;;           '(9 16 36)))

;; (deftest value.finding-maximizing.2 ()
;;   (should be equal
;;           (iter
;;             (:for i :from 3 :to 6)
;;             (:adjoining
;;              (:finding (* i i)
;;               :maximizing (integer-length i)
;;               :into foo) :test #'=))
;;           (9 16)))

(deftest walk.counting ()
  (should be equal
          (iter
            (:for i :from 3 :to 5)
            (:counting (:if-first-time nil t)))
          2))

;; (deftest value.counting ()
;;   (should be equal
;;           (iter
;;             (:for x :in-sequence '#(nil t nil t))
;;             (:collect (:counting x :into foo)))
;;           '(0 1 1 2)))

(deftest value.adjoining ()
  (should be equal
          (iter
            (:for i :from 3 :to 5)
            (:sum (length (:adjoining i :into foo))))
          6))

(deftest value.collecting ()
  (should be equal
          (iter
            (:for i :from 3 :to 5)
            (:collect (copy-list (:collecting i :into foo :at #:start))
              :at end))
          '((3) (4 3) (5 4 3))))

;; FIXME in-string is broken
;; (deftest value.accumulate ()
;;   (should be equal
;;           (iter
;;             (:for c :in-string "245")
;;             (:collect (:accumulate (digit-char-p c) :by #'+
;;                        :initial-value 0 :into s) :into l)
;;             (:finally (return (cons s l))))
;;           '(11 2 6 11)))

(deftest value.always ()
  (should be equal
          (iter
            (:for i :from -3 :downto -6 :by 2)
            (:summing (:always i) :into x)
            (:finally (return x)))
          -8))

(deftest dotted.1 ()
  (should be equal
          (iter
            (:for l :on '(1 2 . 3))
            (:collect l))
          '((1 2 . 3) (2 . 3))))

(deftest dotted.2 ()
  (should be equal
          (iter
            (:for (e) :on '(1 2 . 3))
            (:collect e))
          '(1 2)))

;; FIXME RUTILS:ITER should signal error here.
;;
;; e.g. in CL, (cl:loop for i in '(1 2 . 3) do (print i)) signals "3 is not of type list."
(deftest dotted.3 ()
  (should be equal
          (values
           (ignore-errors
            (iter
              (:for i :in '(1 2 . 3))
              (declare (ignorable i))
              (:count t))))
          nil))

(deftest dotted.4 ()
  (should be equal
          (iter (:for i :in '(1 1 2 3 . 3)) (:thereis (evenp i)))
          t))

(deftest dotted.5 ()
  (should be equal
          (iter (:for i :in '(1 2 . 3)) (:thereis (evenp i)))
          t))

;; (deftest walk.multiple-value-bind ()
;;   (should be equal
;;           (string-upcase
;;            (iter (:for name :in-vector (vector 'iter "FoOBaRzOt" '#:repeat))
;;              (multiple-value-bind (sym access)
;;                  (find-symbol (string name) #.*package*)
;;                (declare (type symbol sym))
;;                (:collect (if access (char (symbol-name sym) 0) #\-)
;;                  :result-type string))))
;;           "I-R"))

(deftest subblocks.1 ()
  (should be equal
          (iter fred
            (:for i :from 1 :to 10)
            (iter barney
              (:for j :from i :to 10)
              (if (> (* i j) 17)
                  (:return-from fred j))))
          9))

(deftest subblocks.wrong.1 ()
  (should be equal
          (let ((ar #2a((1 2 3)
                        (4 5 6)
                        (7 8 9))))
            (iter (:for i :below (array-dimension ar 0))
              (iter (:for j :below (array-dimension ar 1))
                (:collect (aref ar i j)))))
          nil))

(deftest subblocks.2 ()
  (should be equal
          (let ((ar #2a((1 2 3)
                        (4 5 6)
                        (7 8 9))))
            (iter outer (:for i :below (array-dimension ar 0))
              (iter (:for j :below (array-dimension ar 1))
                (:in outer (:collect (aref ar i j))))))
          '(1 2 3 4 5 6 7 8 9)))

(deftest destructuring.1 ()
  (should be equal
          (iter (:for (values (a . b) c)
                 := (funcall #'(lambda () (values (cons 1 'b) 2))))
            (:leave (list a b c)))
          '(1 b 2)))

(deftest :leave ()
  (should be equal
          (iter (:for x :in '(1 2 3))
            (if (evenp x) (:leave x))
            (:finally (error "not found")))
          2))

(deftest nil.block.names ()
  (should be equal
          (iter (:for x :in '(1 2 3))
            (dolist (y '(5 3 4))
              (when (= x y)
                (:leave x)))
            (:finally (error "not found")))
          3))

;; FIXME :index-of-sequence is broken
;; (deftest lambda ()
;;   (should be equal
;;           (iter
;;             (:for i :index-of-sequence "ab")
;;             (:collecting ((lambda(n) (cons 1 n)) i)))
;;           '((1 . 0) (1 . 1))))

(deftest type.1 ()
  (should be equal
          (iter (:for el :in '(1 2 3 4 5))
            (declare (fixnum el))
            (:counting (oddp el)))
          3))

(deftest type.2 ()
  (should be equal
          (iter (:for (the fixnum el) :in '(1 2 3 4 5))
            (:counting (oddp el)))
          3))

;; FIXME What is iter:declare-variables ?
;; (deftest type.3 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:for el :in '(1 2 3 4 5))
;;             (:count (oddp el) :into my-result)
;;             (declare (integer my-result))
;;             (:finally (return my-result)))
;;           3))

;; FIXME What is iter:declare-variables ?
;; (deftest type.4 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:for i :from 1 :to 10)
;;             (:collect i))
;;           '(1 2 3 4 5 6 7 8 9 10)))

;; FIXME What is iter:declare-variables ?
;; (deftest type.5 ()
;;   (should be equal
;;           (iter
;;             (declare (iter:declare-variables))
;;             (:repeat 0)
;;             (:minimize (the fixnum '1)))
;;           0))

;; FIXME What is iter:declare-variables ?
;; (deftest type.6 ()
;;   (should be equal
;;           (iter
;;             (declare (iter:declare-variables))
;;             (:repeat 0)
;;             (:maximize 1))
;;           0))

;; FIXME What is iter:declare-variables ?
;; (deftest type.7 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:repeat 0)
;;             (:minimize (the double-float '1.0d0)))
;;           0.0d0))

;;; this test catches problems :with make-initial-value, which cannot find a good initial
;;; value :for cases where the initial value should be NIL.  This causes generati:on of a
;;; spurious warning.

;; (deftest type.8 ()
;;   (should be equal
;;           (catch 'warned
;;             (handler-bind ((simple-warning #'(lambda (w) (throw 'warned (format nil "~a" w))))
;;                            (error #'(lambda (w) (throw 'warned (format nil "~a" w)))))
;;               (let ((vec (vector (make-instance 'polar :mag 2) (make-instance 'polar :mag 4))))
;;                 (nth-value 1
;;                            (iter (:for x :in-vector vec :with-index i)
;;                              (declare (type (or null polar) x) (type fixnum i))
;;                              (with-slots (rho) x
;;                                (:finding x :such-that (= rho 4) :into target))
;;                              (:finally (return (values target i))))))))
;;           1))

;;; test that :counting :result-type uses an initform of the appropriate type.
;; FIXME Unknown keyword type :result-type
;; (deftest type.9 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:repeat 0)
;;             (:counting t :result-type double-float))
;;           0d0))

;;; test that :sum :result-type uses an accumulator of the appropriate type.
;; FIXME Unknown keyword type :result-type
;; (deftest type.10 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:repeat 2)
;;             (:sum most-positive-fixnum :result-type integer))
;;           #.(* 2 most-positive-fixnum)))

;;; test that multiply :result-type uses an accumulator of the appropriate type.
;; FIXME Unknown keyword type :result-type
;; (deftest type.11 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:for n :in (list most-positive-fixnum 2))
;;             (multiply n :result-type integer))
;;           #.(* 2 most-positive-fixnum)))

;;; test that :reducing :result-type :with an initform doesn't error
;; FIXME Unknown keyword type :result-type
;; (deftest type.12 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:for i :from 1 :to 4)
;;             (:reducing i :by #'+ :result-type fixnum))
;;           10))

;;; test that :reducing :result-type uses an initform of the appropriate type.
;; the manual says that this behavior is undefined, but it does work, and it's necessary
;; :for sbcl :to :generate efficient numeric reductions without throwing a fit.
;; FIXME Unknown keyword type :result-type
;; (deftest type.13 ()
;;   (should be equal
;;           (iter (declare (iter:declare-variables))
;;             (:repeat 0)
;;             (:reducing 0d0 :by #'+ :result-type double-float))
;;           0d0))

(deftest static.error.1 ()
  (should be equal
          (values
           (ignore-errors  ; Iter complains multiple values make no sense here
            (macroexpand-1 '(iter (:for (values a b) :in '(1 2 3)))) t))
          nil))

(deftest code-movement.1 ()
  (should be equal
          (handler-case (macroexpand
                         '(iter (:for i :from 1 :to 10)
                           (let ((x 3))
                             (:initially (setq x 4))
                             (return x))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.2 ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((x 3))
                                         (:collect i :into x))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.3 ()
  (should be equal
          (iter (:with x := 3)
            (:for el :in '(0 1 2 3))
            (setq x 1)
            (:reducing el :by #'+ :initial-value x))
          ;; not 7
          9))

(deftest code-movement.else ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((x 3))
                                         (:else (return x)))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.after-each ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (:after-each (princ y)))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.declare.1 ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (declare (optimize safety))
                                         (:after-each (princ y)))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.declare.2 ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((safety i))
                                         (after-each
                                          (let ()
                                            (declare (optimize safety))
                                            (princ i))))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          nil))

(deftest code-movement.locally.1 ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (:else (locally (princ y))))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.locally.2 ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (:else (locally (princ i))))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          nil))

(deftest code-movement.initially ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (:initially (princ y)))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.finally ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (:finally (return y)))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest code-movement.finally-protected ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (let ((y i))
                                         (:finally-protected (return y)))))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

(deftest static.conflict.1 ()
  (should be equal
          (handler-case (macroexpand '(iter (:for i :from 1 :to 10)
                                       (:collect i) (:sum i)))
            (error () t)
            (:no-error (f x) (declare (ignore f x)) nil))
          t))

;;; 2005: I'm considering making this shadowing feature unspecified (and
;;; removing the test), because it takes away implementati:on freedom of
;;; choosing :to reimplement Iter's own clauses via macrolet or defmacro.

;; (deftest macro.shadow.clause ()
;;   (should be equal
;;           (macrolet ((multiply (expr)
;;                        `(:reducing ,expr :by #'+ :initial-value 0)))
;;             (iter (:for el :in '(1 2 3 4))
;;               (multiply el)))
;;           10))

(deftest multiply.1 ()
  (should be equal
          (iter (:for el :in '(1 2 3 4))
            (:multiply el))
          24))

(defmacro sum-of-squares (expr)
  (let ((temp (gensym)))
    `(let ((,temp ,expr))
       (:sum (* ,temp ,temp)))))

(deftest sum-of-squares.1 ()
  (should be equal
          (iter (:for el :in '(1 2 3))
            (sum-of-squares el))
          14))

(deftest defmacro-clause.1 ()
  (should be equal
          (defmacro-clause (multiply.clause expr &optional :INTO var)
            ":from testsuite"
            `(:reducing ,expr :by #'* :into ,var :initial-value 1))
          ;; A better :return value would be the exact list usable :with remove-clause
          ;; The :next versi:on shall do that
          (multiply.clause expr &optional :INTO var)))

(deftest multiply.clause ()
  (should be equal
          (iter (:for el :in '(1 2 3 4))
            (multiply.clause el))
          24))

;; FIXME What is iter::remove-clause
;; (deftest remove-clause.1 ()
;;   (should be equal
;;           (iter::remove-clause '(multiply.clause &optional INTO))
;;           t))

;; FIXME What is iter::remove-clause
;; (deftest remove-clause.2 ()
;;   (should be equal
;;           (values
;;            (ignore-errors
;;             (iter::remove-clause '(multiply.clause &optional INTO))))
;;           nil))

;; FIXME These two forms have to be rewritten in RUTILS' way.
;;
;; (iter:defmacro-clause (:for var IN-WHOLE-VECTOR.clause v)
;;   "All the elements of a vector (disregards fill-pointer)"
;;   (let ((vect (gensym "VECTOR"))
;;         (index (gensym "INDEX")))
;;     `(progn
;;        (:with ,vect := ,v)
;;        (:for ,index :from 0 :below (array-dimension ,vect 0))
;;        (:for ,var := (aref ,vect ,index)))))
;; (deftest in-whole-vector.clause ()
;;   (should be equal
;;           (iter (:for i IN-WHOLE-VECTOR.clause (make-array 3 :fill-pointer 2
;;                                                              :initial-contents '(1 2 3)))
;;             (:collect i))
;;           (1 2 3)))

;; (deftest in-vector.fill-pointer ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector (make-array 3 :fill-pointer 2
;;                                              :initial-contents '(1 2 3)))
;;             (:collect i))
;;           (1 2)))

;; (iter:defmacro-driver (:for var IN-WHOLE-VECTOR v)
;;   "All the elements of a vector (disregards fill-pointer)"
;;    (let ((vect (gensym "VECTOR"))
;;          (end (gensym "END"))
;;          (index (gensym "INDEX"))
;;          (kwd (if iter:generate ':generate 'for)))
;;      `(progn
;;         (:with ,vect := ,v)
;;         (:with ,end := (array-dimension ,vect 0))
;;         (:with ,index := -1)
;;         (,kwd ,var :next (progn (incf ,index)
;;                                (if (>= ,index ,end) (:terminate))
;;                                (aref ,vect ,index))))))

;; (deftest in-whole-vector.driver ()
;;   (should be equal)
;;   (iter (:for i IN-WHOLE-VECTOR (make-array '(3) :fill-pointer 2
;;                                                  :initial-contents '(1 2 3)))
;;     (:collect i))
;;   (1 2 3))

;; (deftest in-whole-vector.generate ()
;;   (should be equal)
;;   (iter (:generating i IN-WHOLE-VECTOR (make-array '(3) :fill-pointer 2
;;                                                         :initial-contents '(1 2 3)))
;;     (:collect (:next i)))
;;   (1 2 3))

;; (deftest defclause-sequence ()
;;   (should be equal)
;;   (progn
;;     (iter:defclause-sequence IN-WHOLE-VECTOR.seq INDEX-OF-WHOLE-VECTOR
;;       :access-fn 'aref
;;       :size-fn '#'(lambda (v) (array-dimension v 0))
;;       :sequence-type 'vector
;;       :element-type t
;;       :element-doc-string
;;       "Elements of a vector, disregarding fill-pointer"
;;       :index-doc-string
;;       "Indices of vector, disregarding fill-pointer")
;;     t)
;;   t)

;; (deftest in-whole-vector.seq ()
;;   (should be equal)
;;   (iter (:for i IN-WHOLE-VECTOR.seq (make-array '(3) :fill-pointer 2
;;                                                      :initial-contents '(1 2 3)))
;;     (:collect i))
;;   (1 2 3))

;; (deftest in-whole-vector.seq.index ()
;;   (should be equal)
;;   (iter (:for i INDEX-OF-WHOLE-VECTOR
;;               (make-array 3 :fill-pointer 2 :initial-contents '(1 2 3)))
;;     (:for j :previous i :initially 9)
;;     (:collect (list j i)))
;;   ((9 0)(0 1)(1 2)))

;; (deftest in-whole-vector.seq.with-index ()
;;   (should be equal)
;;   (iter (:for e IN-WHOLE-VECTOR.seq
;;          (make-array '(3) :fill-pointer 2 :initial-contents '(a b c))
;;          :with-index i)
;;     (:for j :previous i :initially 9)
;;     (:collect (list j i e)))
;;   ((9 0 a)(0 1 b)(1 2 c)))

;; (deftest in-whole-vector.seq.generate ()
;;   (should be equal)
;;   (iter (:generate e IN-WHOLE-VECTOR.seq
;;          (make-array 3 :fill-pointer 2 :initial-contents '(a b c))
;;          :with-index i)
;;     (:collect (list (:next e) e i)))
;;   ((a a 0) (b b 1) (c c 2)))

;; ;; The original example had three bugs:
;; ;; - ,expr and ,func occured twice :in expansion
;; ;; - (:finally (:leave ,winner)) breaks because :FINALLY does not walk
;; ;;   its forms, so :LEAVE does not work inside :FINALLY.
;; ;; - Do not use (:finally (RETURN ,winner)) either, :as that would
;; ;;   :always :return accumulated value, even :in case of ... :INTO nil.
;; (deftest defmacro-clause.2 ()
;;   (should be equal)
;;   (defmacro-clause (:FINDING expr MAXING func &optional :INTO var)
;;     "Iter paper demo example"
;;     (let ((max-val (gensym "MAX-VAL"))
;;           (temp1 (gensym "EL"))
;;           (temp2 (gensym "VAL"))
;;           (winner (or var iter:*result-var*)))
;;       `(progn
;;          (:with ,max-val := nil)
;;          (:with ,winner := nil)
;;          (let* ((,temp1 ,expr)
;;                 (,temp2 (funcall ,func ,temp1)))
;;            (when (or (null ,max-val) (> ,temp2 ,max-val))
;;              (setq ,winner ,temp1 ,max-val ,temp2)))
;;          #|(:finally (return ,winner))|# )))
;;   (:FINDING expr MAXING func &optional :INTO var))

;; (deftest maxing.1 ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector #(1 5 3))
;;             (:finding i :maxing #'identity))
;;           5))

;; (deftest maxing.2 ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector #(1 5 3))
;;             (:finding i maxing #'identity :into foo))
;;           nil))

;; (deftest maxing.3 ()
;;   (should be equal
;;           (iter
;;             (:for i :in-vector #(2 5 4))
;;             (:finding i maxing #'identity :into foo)
;;             (when (evenp i) (:sum i)))
;;           6))

(deftest display.1 ()
  (should be equal
          (let ((*standard-output* (make-broadcast-stream)))
            (display-iter-clauses) t)
          t))

(deftest display.2 ()
  (should be equal
          (let ((*standard-output* (make-broadcast-stream)))
            (display-iter-clauses 'for) t)
          t))

(deftest multiple-value-prog1.1 ()
  (should be equal
          (iter
            (:for x :in '(a b c))
            (declare (ignorable x))
            (:collect (multiple-value-prog1 7)))
          '(7 7 7)))

(deftest ignore-errors.1 ()
  (should be equal
          (iter
            (:for x :in '(a b c))
            (:collect (ignore-errors x)))
          '(a b c)))

;; (deftest ignore-errors.2 ()
;;   (should be equal
;;           (iter
;;             (:generate x :in '(a b c))
;;             (:collect (ignore-errors (:next x))))
;;           '(a b c)))

(deftest handler-bind.1 ()
  (should be equal
          (iter
            (:for i :from -1 :to 2 :by 2)
            (handler-bind ((error (lambda(c) c nil)))
              (:collect i)))
          '(-1 1)))

(deftest destructuring-bind.1 ()
  (should be equal
          ;; One versi:on of Iter would enter endless loop :in ACL 7 and SBCL
          ;; reported :by Julian Stecklina :in early 2005
          (null (macroexpand '(iter (:for index :in '((1 2)))
                               (:collect (destructuring-bind (a b) index
                                           (+ a b))))))
          nil))

(deftest destructuring-bind.2 ()
  (should be equal
          (iter
            (:for index :in '((1 2)))
            (:collect
                (destructuring-bind (a b)
                    index
                  (+ a b))))
          '(3)))

(deftest symbol-macrolet ()
  (should be equal
          (iter
            (:for i :from -1 :downto -3)
            (symbol-macrolet ((x (* i i)))
              (declare (optimize debug))
              (:sum x)))
          14))

(defclass polar ()
  ((rho   :initarg :mag)
   (theta :initform 0 :accessor angle)))

;; (deftest with-slots ()
;;   (should be equal
;;           (iter (:with v := (vector (make-instance 'polar :mag 2)))
;;             (:for x :in-sequence v)
;;             (with-slots (rho) x
;;               (:multiplying rho)))
;;           2))

;; (deftest with-accessors ()
;;   (should be equal
;;           (iter
;;             (:with v := (vector (make-instance 'polar :mag 1)))
;;             (:for x :in-sequence v)
;;             (with-accessors ((alpha angle)) x
;;               (incf alpha 2)
;;               (:summing alpha)))
;;           2))

;;; Tests for bugs.
;; when these start failing, I have done something right (-:

;; The walker ignores function bindings,
;; therefore shadowing is not handled correctly.
;; (deftest bug/walk.1 ()
;;   (should be equal
;;           (macrolet ((over (x) `(:collect ,x)))
;;             (iter
;;               (:for i :in '(1 2 3))
;;               (flet ((over (x)
;;                        (declare (ignore x))
;;                        (:collect 1)))
;;                 (over i))))             ; would yield (1 1 1) if correct
;;           (1 2 3)))

(deftest bug/walk.2 ()
  (should be equal
          (iter (return (if (oddp 1)
                             (progn)
                             'even)))
          ;; The bug is :in emtpy PROGN walking. Due :to that the :THEN branch is lost
          ;; and it returns 'EVEN instead of NIL.
          nil))

;; FIXME :previous keyword is missing
;;
;; (deftest bug/previously-initially.1 ()
;;   (should be equal
;;           (values
;;            (ignore-errors
;;             ;; It used :to silently ignore the entire :previous expression
;;             ;; and :return (0 0). Now it signals a compile-time error.
;;             (iter
;;               (:repeat 2)
;;               (:for x :previous (zork foo) :initially 0)
;;               (:collect x))
;;             'it-should-have-errored))
;;           nil))

;; FIXME :previous keyword is missing
;;
;; (deftest bug/previously-initially.2 ()
;;   (should be equal
;;           (let ((first-arg 1)
;;                 (more-args '(2 3 4)))
;;             (iter outer
;;               (:for rest-args :on more-args)
;;               (:for tmp := (car rest-args))
;;               (:for first :previous tmp :initially first-arg)
;;               (iter (:for second :in rest-args)
;;                 (:in outer (:collect (list first second))))))
;;           '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))))

;; (deftest bug/macrolet.2 ()
;;   (should be equal
;;           (progn
;;             (format *error-output*
;;                     "~&Note: These tests :generate warnings ~
;;   involving MACROLET with:in Iter~%")
;;             (values
;;              (ignore-errors             ; would yield 1 if correct
;;               (iter (:repeat 10)
;;                 (macrolet ((foo () 1))
;;                   (:multiplying (foo)))))))
;;           nil))

;; FIXME MACROLET is not permitted inside Iterate. Please refactor the Iterate
;; form (e.g. by using macrolets that wrap the ITERATE form).
;;
;; (deftest macrolet.3 ()
;;   (should be equal
;;           (iter
;;             (:repeat 2)
;;             (:multiplying
;;              (macrolet ((foo () 1))
;;                (foo))))
;;           1))

;; FIXME
;; Test BUG/COLLECT-AT-BEGINNING:
;; '(9 7 5 3 1 2 4 6 8 10) FAIL
;; error: #<TYPE-ERROR expected-type: CONS datum: NIL>
;;   FAILED
;;
;; (deftest bug/collect-at-beginning ()
;;   (should be equal
;;           (iter
;;             (:for i :from 1 :to 10)
;;             (if (oddp i)
;;                 (:collect i :at :beginning)
;;                 (:collect i)))
;;           '(9 7 5 3 1 2 4 6 8 10)))

;; Hashtable iterators are specified :to be defined :as macrolets.
;; But we handle these :by special-casing with-hash-table/package-iterator
(deftest nested-hashtable.1 ()
  (should be equal
          (let ((ht1 (make-hash-table))
                (ht2 (make-hash-table)))
            (setup-hash-table ht2)
            (setf (gethash 'a ht1) ht2)
            (= (hash-table-count ht2)
               (length
                (iter outer (:for (k1 v1) :in-hashtable ht1)
                  (iter (:for (k2 v2) :in-hashtable ht2)
                    (:in outer (:collect k2)))))))
          t))

(deftest nested.in-hashtable.2 ()
  (should be equal
          ;; Here the inner macrolet code does not affect the outer iteration
          (let ((ht1 (make-hash-table))
                (ht2 (make-hash-table)))
            (setup-hash-table ht2)
            (setf (gethash 'a ht1) ht2)
            (iter (:for (k1 v1) :in-hashtable ht1)
              (counting
               (iter (:for (k2 nil) :in-hashtable ht2)
                 (:count k2)))))
          1))

(deftest nested.in-hashtable.3 ()
  (should be equal
          (let ((ht1 (make-hash-table))
                (ht2 (make-hash-table)))
            (setup-hash-table ht2)
            (setf (gethash 'a ht1) ht2)
            (iter (:for (k1 v1) :in-hashtable ht1)
              (progn
                (iter (:for (nil v2) :in-hashtable v1)
                  (:count v2))
                (:collect k1))))
          (a)))

(deftest nested.in-package ()
  (should be equal
          (< 6
             (print
              (iter
                (:for scl :in-package '#:iter :external-only t)
                (:count                 ; Iter exports ~50 symbols
                 (iter
                   (:for si :in-package #.*package*)
                   (:thereis (eq si scl))))))
             80)
          t))

(deftest macrolet.loop-finish ()
  (should be equal
          (iter
            (:for l :on *an-alist*)
            (loop :for e :in l
                  :when (equal (car e) 'zero)
                    :do (loop-finish)))
          nil))

;; Misc tests :to make sure that bugs don't reappear

(defmacro problem-because-i-return-nil (&rest args)
  (declare (ignore args))
  nil)

(deftest tagbody.nil-tags ()
  (should be equal
          ;;  Allegro (correctly) won't compile when a tag (typically NIL) is used more than once :in a tagbody.
          (labels ((find-tagbody (form)
                     (cond
                       ((and (consp form)
                             (eq (first form)
                                 'tagbody))
                        form)
                       ((consp form)
                        (iter (:for x :in (rest form))
                          (:thereis (find-tagbody x))))
                       (t nil)))
                   (all-tagbody-tags (form)
                     (iter (:for tag-or-form :in (rest (find-tagbody form)))
                       (when (symbolp tag-or-form)
                         (:collect tag-or-form)))))
            (let* ((form (macroexpand '
                          (iter (:for x :in '(1 2 3))
                            (problem-because-i-return-nil)
                            (+ x x)
                            (problem-because-i-return-nil))))
                   (tags (all-tagbody-tags form)))
              (iter (:for tag :in tags)
                ;; invoke cl:count, not the Iter clause:
                (:always (= 1 (funcall #'count tag tags :from-end nil))))))
          t))

;; (deftest walk.tagbody.1 ()
;;   (should be equal
;;           (iter
;;             (tagbody
;;                (problem-because-i-return-nil)
;;              3
;;                (problem-because-i-return-nil)
;;                (:leave 2)))
;;           2))

(deftest walk.tagbody.2 ()
  (should be equal
          (symbol-macrolet ((error-out (error "do not expand me")))
            (iter (tagbody error-out
                     (:leave 2))))
          2))

#+ccl
(deftest ccl-compiler-let ()
  (should be equal
          (catch 'compiler-warned
            (handler-bind
                ((ccl:compiler-warning #'(lambda (e)
                                           (declare (ignore e))
                                           (throw 'compiler-warned nil))))
              (let ((def (compile nil '(lambda (list-xs)
                                        (iter (:for x :in list-xs) (assert x () "~s is not a foo." x))))))
                (when def t))))
          t))

#+allegro
(deftest allegro-compiler-let ()
  (should be equal
          (catch 'compiler-warned
            (handler-bind
                ((warning #'(lambda (e)
                              (declare (ignore e))
                              (throw 'compiler-warned nil))))
              (let ((def (compile nil '(lambda (list-xs)
                                        (iter (:for x :in list-xs) (assert x () "~s is not a foo." x))))))
                (when def t))))
          t))


(define-condition unexpected-failures-error (error)
  ((failures :initarg :failures))
  (:report (lambda (ufe str)
               (format str "Unexpected failures:~%~{~a~%~}" (slot-value ufe 'failures)))))

(define-condition unexpected-successes-error (error)
  ((successes :initarg :successes))
  (:report (lambda (ufe str)
               (format str "Unexpected successes:~%~{~a~%~}" (slot-value ufe 'successes)))))

;; TODO Remove this. Reason: We don't need.
;;
;; (defun do-iter-tests (&key (on-failure :error))
;;   (multiple-value-bind (unexpected-failures unexpected-successes)
;;       (let ((*expected-failures* *tests-expected-to-fail*)
;;             #-sbcl(*expanded-eval* t)
;;             (*compile-tests* nil))
;;         (do-tests)
;;         (values
;;          (set-difference (pending-tests) *tests-expected-to-fail*)
;;          (set-difference *tests-expected-to-fail* (pending-tests))))
;;     (if (or unexpected-failures unexpected-successes)
;;      (format t "~&DO-ITER-TESTS returned~@[ ~S unexpected failure(s)~]~:[~; and~]~@[ ~S unexpected success(es)~].~%"
;;              unexpected-failures (and unexpected-failures unexpected-successes) unexpected-successes)
;;      (format t "~&DO-ITER-TESTS ran successfully.~%"))
;;     (cond
;;       ((and unexpected-failures
;;             (eq on-failure :error))
;;        (error 'unexpected-failures-error :failures unexpected-failures))
;;       (unexpected-failures
;;        on-failure)
;;       ;; no unexpected failures
;;       ((and unexpected-successes
;;            (eq on-failure :error))
;;        (error 'unexpected-successes-error :successes unexpected-successes))
;;       (unexpected-successes
;;        on-failure)
;;       (t t))))

;;; eof
