(defsystem "pairing-heap"
  :description "Pairing heap implementaton"
  :author "Jānis Džeriņš <lisp@jonis.lv>"
  :license "Zlib"
  :long-description "Straight implementation of pairing heap, as described
  in https://en.wikipedia.org/wiki/Pairing_heap."
  :components ((:file "package")
               (:file "pairing-heap" :depends-on ("package")))
  :in-order-to ((test-op (test-op "pairing-heap/tests"))))

(defsystem "pairing-heap/tests"
  :depends-on ("pairing-heap")
  :components ((:file "tests"))
  :perform (test-op (operation component)
                    (symbol-call '#:pairing-heap.tests '#:run-tests)))
