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
#+pairing-heap/use-structs
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

#-pairing-heap/use-structs
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

(defun print-pairing-tree (tree &optional (stream *standard-output*))
  (labels ((walk-node (node prefix lastp)
             (format stream "~&~{~A~}~:[├~;└~]─ ~A~%"
                     prefix lastp (tree-elem node))
             (walk-children (tree-subheaps node)
                            (append prefix (list (if lastp "   " "│  ")))))
           (walk-children (list prefix)
             (loop for (node . more) on list
                   do (walk-node node prefix (null more)))))
    (format stream "~&~A~%" (tree-elem tree))
    (walk-children (tree-subheaps tree) nil)
    tree))

(defstruct (pairing-heap
            (:constructor %make-pairing-heap (key test))
            (:conc-name heap-))
  (root nil :type (or null pairing-tree))
  (key #'identity :type function :read-only t)
  (test #'< :type function :read-only t))

(defun make-heap (&key (key #'identity)
                       (test #'<)
                       (initial-contents '()))
  (let ((heap (%make-pairing-heap (coerce key 'function)
                                  (coerce test 'function))))
    (dolist (elem initial-contents)
      (insert elem heap))
    heap))

(defun heap-depth (heap)
  (labels ((walk (tree depth)
             (let ((subs (tree-subheaps tree)))
               (if subs
                   (loop for sub in (tree-subheaps tree)
                         maximizing (walk sub (1+ depth)))
                   depth))))
    (let ((root (heap-root heap)))
      (if root
          (walk root 1)
          0))))

(define-condition empty-heap-error ()
  ((heap
    :initarg :heap
    :reader empty-heap-error-heap))
  (:report
   (lambda (condition stream)
     (format stream "Heap ~S is empty."
             (empty-heap-error-heap condition)))))

(defun find-min (heap)
  (declare (type pairing-heap heap))
  (let ((root (heap-root heap)))
    (if root
        (tree-elem root)
        (error 'empty-heap-error :heap heap))))

(defun meld-trees (one two key test)
  (declare (type pairing-tree one two)
           (type function key test)
           (optimize speed))
  (let ((elem-one (tree-elem one))
        (elem-two (tree-elem two)))
    (if (funcall test
                 (funcall key elem-one)
                 (funcall key elem-two))
        (make-pairing-tree elem-one (list* two (tree-subheaps one)))
        (make-pairing-tree elem-two (list* one (tree-subheaps two))))))

(defun merge-pairs (subheaps key test)
  (declare (type list subheaps)
           (type function key test))
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

(defun insert (elem heap)
  (declare (type pairing-heap heap))
  (let ((root (heap-root heap))
        (new (make-pairing-tree elem '())))
    (setf (heap-root heap)
          (if root
              (meld-trees root new (heap-key heap) (heap-test heap))
              new))))

(defun delete-min (heap)
  (declare (type pairing-heap heap))
  (let ((root (heap-root heap)))
    (if root
        (prog1 (tree-elem root)
          (setf (heap-root heap)
                (merge-pairs (tree-subheaps root)
                             (heap-key heap)
                             (heap-test heap))))
        (error 'empty-heap-error :heap heap))))

(defun emptyp (heap)
  (null (heap-root heap)))
