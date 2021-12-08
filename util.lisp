(in-package #:simple-pairing-heap)

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

(defun print-heap (heap &optional (stream *standard-output*))
  (labels ((walk-node (node prefix lastp)
             (format stream "~&~{~A~}~:[├~;└~]─ ~A~%"
                     prefix lastp (tree-elem node))
             (walk-children (tree-subheaps node)
                            (append prefix (list (if lastp "   " "│  ")))))
           (walk-children (list prefix)
             (loop for (node . more) on list
                   do (walk-node node prefix (null more)))))
    (let ((root (heap-root heap)))
      (when root
        (format stream "~&~A~%" (tree-elem root))
        (walk-children (tree-subheaps root) nil)))
    heap))
