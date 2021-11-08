(in-package #:pairing-heap)

;;; https://en.wikipedia.org/wiki/Pairing_heap
;;;
;;; TODO:
;;;
;;; - SUBHEAPS are actually subtrees.
;;;
;;; - We might want to extract KEY at node construction time.
;;;
;;; - The recursive melding *will* exhaust stack with big enough heaps.
;;;
#+(or ccl pairing-heap/use-structs)
(progn
  (defstruct (pairing-tree
              (:constructor make-pairing-tree (elem subheaps))
              (:conc-name tree-))
    (elem (error "Cannot have PAIRING-TREE without element."))
    (subheaps '() :type list))

  (defmethod print-object ((tree pairing-tree) stream)
    (if *print-readably*
        (call-next-method)
        (print-unreadable-object (tree stream)
          (format stream "TREE ~A ~:A"
                  (tree-elem tree)
                  (tree-subheaps tree))))))

#-(or ccl pairing-heap/use-structs)
(progn
  (deftype pairing-tree ()
    '(cons t list))

  (declaim (inline make-pairing-tree))
  (defun make-pairing-tree (elem subheaps)
    (cons elem subheaps))

  (declaim (inline tree-elem))
  (defun tree-elem (pairing-tree)
    (car pairing-tree))

  (declaim (inline tree-subheaps))
  (defun tree-subheaps (pairing-tree)
    (cdr pairing-tree)))

;;; Performance is abysmal without optimization on CCL.
#+ccl
(declaim (optimize (speed 3) (safety 1)))

(defun meld-trees (one two key test)
  (declare (type pairing-tree one two)
           (type function key test)
           (optimize (speed 3) (safety 1)))
  (let ((elem-one (tree-elem one))
        (elem-two (tree-elem two)))
    (if (funcall test
                 (funcall key elem-one)
                 (funcall key elem-two))
        (make-pairing-tree elem-one (list* two (tree-subheaps one)))
        (make-pairing-tree elem-two (list* one (tree-subheaps two))))))

(defun merge-pairs (subheaps key test)
  (declare (type list subheaps)
           (type function key test)
           (optimize (speed 3) (safety 1)))
  (if (endp subheaps)
      nil
      (let ((one (pop subheaps)))
        (if (endp subheaps)
            one
            (let ((melded-pair (meld-trees one (pop subheaps) key test)))
              (if (endp subheaps)
                  melded-pair
                  (meld-trees melded-pair
                              (merge-pairs subheaps key test)
                              key test)))))))

(defstruct (pairing-heap
            (:constructor %make-pairing-heap (key test))
            (:conc-name heap-))
  (root nil :type (or null pairing-tree))
  (key #'identity :type function :read-only t)
  (test #'< :type function :read-only t))

(defun create (&key (key #'identity)
                    (test #'<)
                    (initial-contents '()))
  (let ((heap (%make-pairing-heap (coerce key 'function)
                                  (coerce test 'function))))
    (dolist (elem initial-contents)
      (insert elem heap))
    heap))

(define-condition empty-heap-error (error)
  ((heap
    :initarg :heap
    :reader empty-heap-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Heap ~S is empty."
             (empty-heap-error-heap condition)))))

(defun empty-heap-error (heap)
  (restart-case
      (error 'empty-heap-error :heap heap)
    (use-value (value)
      :report "Specify a value to use this time."
      :interactive (lambda ()
                     (format *query-io*
                             "~&Please enter a value to use (evaluated): ")
                     (multiple-value-list (eval (read))))
      value)))

(defun front (heap &optional (errorp t) error-value)
  "Returns the front element of the HEAP.  If HEAP is empty and ERRORP is
true (default), then an EMPTY-HEAP-ERROR is signaled; otherwise
ERROR-VALUE is returned."
  (declare (type pairing-heap heap))
  (let ((root (heap-root heap)))
    (cond (root
           (tree-elem root))
          (errorp
           (empty-heap-error heap))
          (t
           error-value))))

(defun insert (elem heap)
  "Inserts ELEM into the HEAP."
  (declare (type pairing-heap heap))
  (let ((root (heap-root heap))
        (new (make-pairing-tree elem '())))
    (setf (heap-root heap)
          (if root
              (meld-trees root new (heap-key heap) (heap-test heap))
              new))))

(defun pop-front (heap &optional (errorp t) error-value)
  "Removes and returns the front element of the HEAP.  If HEAP is empty
and ERRORP is true (default), then an EMPTY-HEAP-ERROR is signaled;
otherwise ERROR-VALUE is returned."
  (declare (type pairing-heap heap))
  (let ((root (heap-root heap)))
    (cond (root
           (prog1 (tree-elem root)
             (setf (heap-root heap)
                   (merge-pairs (tree-subheaps root)
                                (heap-key heap)
                                (heap-test heap)))))
          (errorp
           (empty-heap-error heap))
          (t
           error-value))))

(defun emptyp (heap)
  (null (heap-root heap)))
