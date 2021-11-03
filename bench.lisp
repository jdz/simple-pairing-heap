(in-package #:cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (find-package '#:pairing-heap)
    (ql:quickload "pairing-heap"))
  (unless (find-package '#:bodge-heap)
    (ql:quickload "bodge-heap")))

(defun benchmark (&key (n 1000)
                       (m 1000)
                       (scale (expt n 2))
                       (repeat 50)
                       (warmup 10)
                       (key #'identity)
                       (node-constructor #'identity))
  (let ((init-items (loop repeat n
                          collect (funcall node-constructor (random scale))))
        (more-items (loop repeat m
                          collect (funcall node-constructor (random scale)))))
    (macrolet ((bench ((heap-var item-var description)
                       new add del)
                 `(let ((,heap-var ,new))
                    (fresh-line *trace-output*)
                    (write-line ,description *trace-output*)
                    #+sbcl (sb-ext:gc :full t)
                    (flet ((run ()
                             (loop for ,item-var in init-items
                                   do ,add)
                             (loop for ,item-var in more-items
                                   do ,del ,add)))
                      (loop repeat warmup do (run))
                      (time
                       (loop repeat repeat do (run)))))))
      (bench (heap item "PAIRING-HEAP")
             (pairing-heap:make-heap :key key)
             (pairing-heap:insert item heap)
             (pairing-heap:delete-min heap))
      (bench (heap item "BODGE-HEAP")
             (bodge-heap:make-pairing-heap :key key)
             (bodge-heap:pairing-heap-push heap item)
             (bodge-heap:pairing-heap-pop heap))
      t)))

;;; Bigger heaps hurt us more.  Also we still have not figured out
;;; recursion.

;;; SBCL:

#-(and) (benchmark :n 1000 :m 500000)
;; PAIRING-HEAP
;; Evaluation took:
;;   0.790 seconds of real time
;;   0.796887 seconds of total run time (0.790074 user, 0.006813 system)
;;   [ Run times consist of 0.017 seconds GC time, and 0.780 seconds non-GC time. ]
;;   100.89% CPU
;;   2,991,180,298 processor cycles
;;   2,221,059,760 bytes consed

;; BODGE-HEAP
;; Evaluation took:
;;   0.810 seconds of real time
;;   0.815574 seconds of total run time (0.782068 user, 0.033506 system)
;;   [ Run times consist of 0.011 seconds GC time, and 0.805 seconds non-GC time. ]
;;   100.74% CPU
;;   3,081,919,244 processor cycles
;;   1,202,380,608 bytes consed

#-(and)
(benchmark :n 1000 :m 5000000)
;; PAIRING-HEAP
;; Evaluation took:
;;   7.713 seconds of real time
;;   8.331076 seconds of total run time (8.202528 user, 0.128548 system)
;;   [ Run times consist of 0.797 seconds GC time, and 7.535 seconds non-GC time. ]
;;   108.01% CPU
;;   29,255,630,798 processor cycles
;;   20,304,684,576 bytes consed

;; BODGE-HEAP
;; Evaluation took:
;;   7.800 seconds of real time
;;   7.991809 seconds of total run time (7.927432 user, 0.064377 system)
;;   [ Run times consist of 0.517 seconds GC time, and 7.475 seconds non-GC time. ]
;;   102.46% CPU
;;   29,581,021,606 processor cycles
;;   12,002,376,400 bytes consed

#-(and)
(benchmark :n 50000 :m 1000000)
;; PAIRING-HEAP
;; Evaluation took:
;;   8.523 seconds of real time
;;   8.691516 seconds of total run time (8.157254 user, 0.534262 system)
;;   [ Run times consist of 2.286 seconds GC time, and 6.406 seconds non-GC time. ]
;;   101.98% CPU
;;   32,328,188,894 processor cycles
;;   12,712,230,496 bytes consed

;; BODGE-HEAP
;; Evaluation took:
;;   5.620 seconds of real time
;;   5.660431 seconds of total run time (5.442355 user, 0.218076 system)
;;   [ Run times consist of 0.894 seconds GC time, and 4.767 seconds non-GC time. ]
;;   100.71% CPU
;;   21,318,623,922 processor cycles
;;   2,519,992,736 bytes consed

;;; On CCL our library performance is abysmal.  Probably because of
;;; allocations (even though GC time is less).

#-(and) (benchmark :n 1000 :m 500000)
;; PAIRING-HEAP
;; (LOOP REPEAT REPEAT DO (RUN))
;; took 51,006,103 microseconds (51.006104 seconds) to run.
;;          91,939 microseconds ( 0.091939 seconds, 0.18%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      51,416,257 microseconds (51.416256 seconds) were spent in user mode
;;          80,665 microseconds ( 0.080665 seconds) were spent in system mode
;;  2,225,776,800 bytes of memory allocated.
;;  646 minor page faults, 0 major page faults, 0 swaps.
;; BODGE-HEAP
;; (LOOP REPEAT REPEAT DO (RUN))
;; took 4,293,986 microseconds (4.293986 seconds) to run.
;;        150,619 microseconds (0.150619 seconds, 3.51%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      4,488,338 microseconds (4.488338 seconds) were spent in user mode
;;         27,476 microseconds (0.027476 seconds) were spent in system mode
;;  1,202,400,000 bytes of memory allocated.
;;  459 minor page faults, 0 major page faults, 0 swaps.

;;; Using conses instead of structs for PAIRING-TREE improves the
;;; situation a lot.

#-(and) (benchmark :n 1000 :m 500000)
;; PAIRING-HEAP
;; Evaluation took:
;;   0.693 seconds of real time
;;   0.736250 seconds of total run time (0.727317 user, 0.008933 system)
;;   [ Run times consist of 0.048 seconds GC time, and 0.689 seconds non-GC time. ]
;;   106.20% CPU
;;   2,634,582,788 processor cycles
;;   1,348,995,888 bytes consed

;; BODGE-HEAP
;; Evaluation took:
;;   0.856 seconds of real time
;;   0.895991 seconds of total run time (0.892014 user, 0.003977 system)
;;   [ Run times consist of 0.044 seconds GC time, and 0.852 seconds non-GC time. ]
;;   104.67% CPU
;;   3,248,024,046 processor cycles
;;   1,202,393,200 bytes consed

#-(and) (benchmark :n 1000 :m 5000000)
;; PAIRING-HEAP
;; Evaluation took:
;;   6.040 seconds of real time
;;   6.275750 seconds of total run time (6.228204 user, 0.047546 system)
;;   [ Run times consist of 0.411 seconds GC time, and 5.865 seconds non-GC time. ]
;;   103.91% CPU
;;   22,905,674,808 processor cycles
;;   12,201,050,400 bytes consed

;; BODGE-HEAP
;; Evaluation took:
;;   7.803 seconds of real time
;;   8.138431 seconds of total run time (8.075706 user, 0.062725 system)
;;   [ Run times consist of 0.402 seconds GC time, and 7.737 seconds non-GC time. ]
;;   104.29% CPU
;;   29,605,761,316 processor cycles
;;   12,002,395,600 bytes consed

#-(and) (benchmark :n 50000 :m 1000000)
;; PAIRING-HEAP
;; Evaluation took:
;;   6.156 seconds of real time
;;   6.391795 seconds of total run time (6.120094 user, 0.271701 system)
;;   [ Run times consist of 1.085 seconds GC time, and 5.307 seconds non-GC time. ]
;;   103.83% CPU
;;   23,357,074,300 processor cycles
;;   8,150,554,496 bytes consed

;; BODGE-HEAP
;; Evaluation took:
;;   5.646 seconds of real time
;;   5.729485 seconds of total run time (5.534890 user, 0.194595 system)
;;   [ Run times consist of 0.842 seconds GC time, and 4.888 seconds non-GC time. ]
;;   101.47% CPU
;;   21,420,206,358 processor cycles
;;   2,520,007,616 bytes consed

;;; The CCL situation on the other hand worsens dramatically:

#-(and) (benchmark :n 1000 :m 500000)
;; PAIRING-HEAP
;; (LOOP REPEAT REPEAT DO (RUN))
;; took 108,605,374 microseconds (108.605380 seconds) to run.
;;           61,812 microseconds (  0.061812 seconds, 0.06%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      109,003,363 microseconds (109.003360 seconds) were spent in user mode
;;           40,126 microseconds (  0.040126 seconds) were spent in system mode
;;  1,350,576,000 bytes of memory allocated.
;;  358 minor page faults, 0 major page faults, 0 swaps.
;; BODGE-HEAP
;; (LOOP REPEAT REPEAT DO (RUN))
;; took 4,445,508 microseconds (4.445508 seconds) to run.
;;        159,866 microseconds (0.159866 seconds, 3.60%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      4,760,603 microseconds (4.760603 seconds) were spent in user mode
;;         54,116 microseconds (0.054116 seconds) were spent in system mode
;;  1,202,400,000 bytes of memory allocated.
;;  628 minor page faults, 0 major page faults, 0 swaps.

;;; With a (declaim (optimize (speed 3) (safety 1))) the situation
;;; improves:

#-(and) (benchmark :n 1000 :m 500000)
;; PAIRING-HEAP
;; (LOOP REPEAT REPEAT DO (RUN))
;; took 22,199,548 microseconds (22.199549 seconds) to run.
;;          54,851 microseconds ( 0.054851 seconds, 0.25%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      22,581,168 microseconds (22.581167 seconds) were spent in user mode
;;          37,361 microseconds ( 0.037361 seconds) were spent in system mode
;;  1,350,576,000 bytes of memory allocated.
;;  368 minor page faults, 0 major page faults, 0 swaps.
;; BODGE-HEAP
;; (LOOP REPEAT REPEAT DO (RUN))
;; took 4,487,729 microseconds (4.487729 seconds) to run.
;;        162,523 microseconds (0.162523 seconds, 3.62%) of which was spent in GC.
;; During that period, and with 16 available CPU cores,
;;      4,851,894 microseconds (4.851894 seconds) were spent in user mode
;;         21,497 microseconds (0.021497 seconds) were spent in system mode
;;  1,202,400,000 bytes of memory allocated.
;;  627 minor page faults, 0 major page faults, 0 swaps.

#+sbcl
(defmacro counting-calls ((&rest names) &body body)
  (let ((vars (loop for name in names
                    for var = (make-symbol (format nil "~A-CALL-COUNT" name))
                    collect (cons name var))))
    `(let (,@(loop for (nil . var) in vars
                   collect `(,var 0)))
       (declare ,@(loop for (nil . var) in vars
                        collect `(special ,var)))
       (unwind-protect
            (progn
              ,@(loop for (name . var) in vars
                      for cond = `(locally (declare (special ,var))
                                    (incf (the fixnum ,var))
                                    nil)
                      collect `(trace ,name :condition ,cond))
              ,@body
              (format *trace-output*
                      "~&~@{~S called ~D times.~%~}"
                      ,@(loop for (name . var) in vars
                              collect (list 'quote name)
                              collect `(locally (declare (special ,var))
                                         (the fixnum ,var)))))
         (untrace)))))

#-(and)
(let* ((n 1000)
       (scale (expt n 2))
       (items (loop repeat n for i upfrom 1 collect (+ i (- (random 10) 4))))
       (heap (pairing-heap:make-heap :initial-contents items)))
  (loop repeat 50
        do (counting-calls (pairing-heap::make-pairing-tree)
             (pairing-heap:delete-min heap))))

#-(and)
(let* ((n 1000)
       (scale (expt n 2))
       (items (loop repeat n collect (random scale)))
       (heap (pairing-heap:make-heap :initial-contents items)))
  (pairing-heap::print-pairing-tree (pairing-heap::heap-root heap)))

#-(and)
(let* ((n 1000)
       (scale (expt n 2))
       (items (loop repeat n collect (random scale)))
       (heap (pairing-heap:make-heap :initial-contents items)))
  (loop repeat 100
        do (counting-calls (pairing-heap::make-pairing-tree)
             (pairing-heap:delete-min heap))
           (pairing-heap:insert (random scale) heap)))
