(defpackage #:simple-pairing-heap.tests
  (:use #:common-lisp)
  (:local-nicknames (#:ph #:simple-pairing-heap))
  (:export #:run-tests))

(in-package #:simple-pairing-heap.tests)

(defun test-heap (&key (n 1000)
                       (scale (expt n 2))
                       (node-constructor #'identity)
                       (test #'<)
                       (key #'identity)
                       (recursive nil)
                       (equals #'eql))
  (let* ((items (loop repeat n
                      collect (funcall node-constructor (random scale))))
         (heap (ph:create :test test
                          :recursive-merge recursive)))
    (loop for item in items
          do (ph:insert item heap (funcall key item)))
    (loop for a in (sort (copy-list items) test :key key)
          for b = (ph:pop-front heap)
          do (assert (funcall equals
                              (funcall key a)
                              (funcall key b))))
    (assert (ph:emptyp heap))
    t))

(defun run-tests (&rest args
                  &key (n 10000)
                  &allow-other-keys )
  (declare (ignorable n))
  (loop for recursive in '(t nil)
        do (apply #'test-heap :test #'< :recursive recursive args)
           (apply #'test-heap :test #'> :recursive recursive args)
           (let ((args (list* :key #'car
                              :node-constructor (lambda (n) (cons n nil))
                              args)))
             (apply #'test-heap :test #'< :recursive recursive args)
             (apply #'test-heap :test #'> :recursive recursive args))))
