(defsystem "simple-pairing-heap"
  :description "Pairing heap implementaton"
  :author "Jānis Džeriņš <lisp@jonis.lv>"
  :license "Zlib"
  :long-description "Straight implementation of pairing heap, as described
  in https://en.wikipedia.org/wiki/Pairing_heap."
  :components ((:file "package")
               (:file "pairing-heap" :depends-on ("package")))
  :in-order-to ((test-op (test-op "simple-pairing-heap/tests"))))

(defsystem "simple-pairing-heap/tests"
  :depends-on ("simple-pairing-heap")
  :components ((:file "tests"))
  :perform (test-op (operation component)
                    (symbol-call '#:simple-pairing-heap.tests '#:run-tests)))
