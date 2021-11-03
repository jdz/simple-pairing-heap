(defpackage #:pairing-heap
  (:use #:common-lisp)
  (:export #:create
           #:insert
           #:front
           #:pop-front
           #:emptyp
           #:empty-heap-error
           #:empty-heap-error-heap))
