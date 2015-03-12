#|
  This file is a part of quadtree project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage quadtree-test-asd
  (:use :cl :asdf))
(in-package :quadtree-test-asd)

(defsystem quadtree-test
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:quadtree
               :prove)
  :components ((:module "t"
                :components
                ((:file "quadtree"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
