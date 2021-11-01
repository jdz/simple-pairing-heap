(defpackage #:pairing-heap.tests
  (:use #:common-lisp)
  (:local-nicknames (#:ph #:pairing-heap))
  (:export #:run-tests))

(in-package #:pairing-heap.tests)

(defun test-heap (&key (n 1000)
                       (scale (expt n 2))
                       (node-constructor #'identity)
                       (test #'<)
                       (key #'identity)
                       (equals #'eql))
  (let* ((items (loop repeat n
                      collect (funcall node-constructor (random scale))))
         (heap (ph:make-heap :test test :key key :initial-contents items)))
    (loop for a in (sort (copy-list items) test :key key)
          for b = (ph:find-min heap)
          do (assert (funcall equals
                              (funcall key a)
                              (funcall key b)))
             (ph:delete-min heap))
    (assert (ph:emptyp heap))
    t))

(defun run-tests (&rest args
                  &key (n 10000)
                  &allow-other-keys )
  (declare (ignorable n))
  (apply #'test-heap :test #'< args)
  (apply #'test-heap :test #'> args)
  (let ((args (list* :key #'car
                     :node-constructor (lambda (n)
                                         (cons n (format nil "~R" n)))
                     args)))
    (apply #'test-heap :test #'< args)
    (apply #'test-heap :test #'> args)))
