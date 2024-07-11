(in-package #:simple-pairing-heap)

;;; https://en.wikipedia.org/wiki/Pairing_heap
;;;
;;; TODO:
;;;
;;; - SUBHEAPS are actually subtrees.
;;;
;;; - We might want to extract KEY at node construction time.

;;; Performance is abysmal without this declamation on CCL.  With this
;;; we're approximately twice as fast as implementation in bodge-heap.
#+ccl
(declaim (optimize (speed 3) (safety 1)))

#+(and)
(progn
  (defstruct (pairing-tree
              (:constructor make-pairing-tree (key elem subheaps))
              (:conc-name tree-))
    (key (error "Cannot have PAIRING-TREE without key."))
    (elem (error "Cannot have PAIRING-TREE without element."))
    (subheaps '() :type list))

  (defmethod print-object ((tree pairing-tree) stream)
    (if *print-readably*
        (call-next-method)
        (let ((node (list (tree-key tree)
                          (tree-elem tree)
                          (tree-subheaps tree))))
          (declare (dynamic-extent node))
          (write node :stream stream)))))

#-(and)
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

(defun meld-trees (one two test)
  (declare (type pairing-tree one two)
           (type function test)
           (optimize (speed 3) (safety 1)))
  (let ((key-one (tree-key one))
        (key-two (tree-key two))
        (elem-one (tree-elem one))
        (elem-two (tree-elem two)))
    (if (funcall test key-one key-two)
        (make-pairing-tree key-one elem-one (list* two (tree-subheaps one)))
        (make-pairing-tree key-two elem-two (list* one (tree-subheaps two))))))

(defun merge-pairs/recursive (subheaps test)
  (declare (type list subheaps)
           (type function test)
           (optimize (speed 3) (safety 1)))
  (if (endp subheaps)
      nil
      (let ((one (pop subheaps)))
        (if (endp subheaps)
            one
            (let ((melded-pair (meld-trees one (pop subheaps) test)))
              (if (endp subheaps)
                  melded-pair
                  (meld-trees melded-pair
                              (merge-pairs/recursive subheaps test)
                              test)))))))

(defun merge-pairs/consing (subheaps test)
  (declare (type list subheaps)
           (type function test)
           (optimize (speed 3) (safety 1)))
  (labels ((ltr (list result)
             (if (endp list)
                 result
                 (let ((one (pop list)))
                   (if (endp list)
                       (cons one result)
                       (let ((two (pop list)))
                         (ltr list
                              (list* (meld-trees one two test)
                                     result)))))))
           (rtl (head tail)
             (if tail
                 (rtl (meld-trees head (first tail) test)
                      (rest tail))
                 head)))
    (let ((melded (ltr subheaps '())))
      (rtl (first melded)
           (rest melded)))))

(defstruct (pairing-heap
            (:constructor %make-pairing-heap (test merge-pairs-fn))
            (:conc-name heap-))
  (root nil :type (or null pairing-tree))
  (test #'< :type function :read-only t)
  (merge-pairs-fn #'merge-pairs/consing :type function :read-only t))

(defun create (&key (test #'<)
                    (recursive-merge nil))
  "Create and return new PAIRING-HEAP.  Items added to the heap will be
sorted using the provided TEST function.  If RECURSIVE-MERGE is
true (default is false) then a recursive MERGE-PAIRS function will be
used to maintain the heap, but this may exhaust call stack on big
heaps."
  (%make-pairing-heap (coerce test 'function)
                      (if recursive-merge
                          #'merge-pairs/recursive
                          #'merge-pairs/consing)))

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
  (let ((root (heap-root heap)))
    (cond (root
           (tree-elem root))
          (errorp
           (empty-heap-error heap))
          (t
           error-value))))

(defun insert (elem heap key)
  "Inserts ELEM into the HEAP."
  (let ((root (heap-root heap))
        (new (make-pairing-tree key elem '())))
    (setf (heap-root heap)
          (if root
              (meld-trees root new (heap-test heap))
              new))))

(defun pop-front (heap &optional (errorp t) error-value)
  "Removes and returns the front element of the HEAP.  If HEAP is empty
and ERRORP is true (default), then an EMPTY-HEAP-ERROR is signaled;
otherwise ERROR-VALUE is returned."
  (let ((root (heap-root heap)))
    (cond (root
           (prog1 (tree-elem root)
             (setf (heap-root heap)
                   (funcall (heap-merge-pairs-fn heap)
                            (tree-subheaps root)
                            (heap-test heap)))))
          (errorp
           (empty-heap-error heap))
          (t
           error-value))))

(defun emptyp (heap)
  (null (heap-root heap)))
