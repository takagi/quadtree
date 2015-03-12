#|
  This file is a part of quadtree project.
  Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage quadtree
  (:use :cl)
  (:export #:*max-depth*
           #:*max-capacity*
           #:make
           #:boundary
           #:insert
           #:query
           #:clear
           #:intersect-p))
(in-package :quadtree)


(defvar *max-depth* 3)

(defvar *max-capacity* 8)

(defstruct (quadtree (:constructor %make-quadtree))
  objects
  nw ne sw se
  boundary
  depth
  max-depth
  max-capacity)

(defun make (x0 y0 x1 y1 &key (max-depth *max-depth*)
                              (max-capacity *max-capacity*))
  (%make x0 y0 x1 y1 0 max-depth max-capacity))

(defun %make (x0 y0 x1 y1 depth max-depth max-capacity)
  (declare (float x0 y0 x1 y1))
  (unless (<= 0 depth max-depth)
    (error "The value ~S is an linvalid value as depth." depth))
  (%make-quadtree :boundary (list x0 y0 x1 y1)
                  :depth depth
                  :max-depth max-depth
                  :max-capacity max-capacity))

(defun boundary (quadtree)
  (quadtree-boundary quadtree))

(defun node-p (quadtree)
  (and (quadtree-nw quadtree)
       t))

(defun leaf-p (quadtree)
  (not (node-p quadtree)))

(defun full-p (quadtree)
  (= (length (quadtree-objects quadtree))
     (quadtree-max-capacity quadtree)))

(defun max-depth-p (quadtree)
  (= (quadtree-depth quadtree)
     (quadtree-max-depth quadtree)))

(defun root-p (quadtree)
  (= (quadtree-depth quadtree) 0))

(defun subdevide (quadtree)
  (unless (leaf-p quadtree)
    (error "The quadtree ~A is already subdevided." quadtree))
  (destructuring-bind (x0 y0 x1 y1) (boundary quadtree)
    (declare (double-float x0 y0 x1 y1))
    (let ((xmid (/ (+ x1 x0) 2.0))
          (ymid (/ (+ y1 y0) 2.0))
          (depth (1+ (the fixnum (quadtree-depth quadtree))))
          (max-depth (quadtree-max-depth quadtree))
          (max-capacity (quadtree-max-capacity quadtree)))
      (setf (quadtree-nw quadtree) (%make x0 ymid xmid y1 depth max-depth max-capacity)
            (quadtree-ne quadtree) (%make xmid ymid x1 y1 depth max-depth max-capacity)
            (quadtree-sw quadtree) (%make x0 y0 xmid ymid depth max-depth max-capacity)
            (quadtree-se quadtree) (%make xmid y0 x1 ymid depth max-depth max-capacity))))
  (loop while (quadtree-objects quadtree)
     do (let ((object (pop (quadtree-objects quadtree))))
          (insert (quadtree-nw quadtree) object)
          (insert (quadtree-ne quadtree) object)
          (insert (quadtree-sw quadtree) object)
          (insert (quadtree-se quadtree) object)))
  t)

(defgeneric intersect-p (quadtree object)
  (:documentation "Returns if the object intersects the quadtree"))

(defun insert (quadtree object)
  (cond
    ;; When the object does not intersect the quadtree, just return nil.
    ((not (intersect-p quadtree object)) nil)
    ;; When the quadtree is a node, recursively insert the object to its
    ;; children.
    ((node-p quadtree) (insert (quadtree-nw quadtree) object)
                       (insert (quadtree-ne quadtree) object)
                       (insert (quadtree-sw quadtree) object)
                       (insert (quadtree-se quadtree) object)
                       t)
    ;; When the quadtree is full and is not at its max depth, subdevide it and
    ;; recursively insert the object.
    ((and (full-p quadtree)
          (not (max-depth-p quadtree)))
     (subdevide quadtree)
     (insert quadtree object))
    ;; Otherwise, insert the object to the quadtree.
    (t (push object (quadtree-objects quadtree))
       t)))

(defun point-intersect-p (quadtree x y)
  (destructuring-bind (x0 y0 x1 y1) (boundary quadtree)
    (and (<= x0 x x1)
         (<= y0 y y1))))

(defun query (quadtree x y &optional neighbor-p)
  (declare (ignore neighbor-p))
  (cond
    ((not (point-intersect-p quadtree x y)) nil)
    ((node-p quadtree)
     (or (query (quadtree-nw quadtree) x y)
         (query (quadtree-ne quadtree) x y)
         (query (quadtree-sw quadtree) x y)
         (query (quadtree-se quadtree) x y)))
    ((leaf-p quadtree)
     (quadtree-objects quadtree))))

(defun clear (quadtree)
  (assert (root-p quadtree))
  (setf (quadtree-objects quadtree) nil
        (quadtree-nw quadtree) nil
        (quadtree-ne quadtree) nil
        (quadtree-sw quadtree) nil
        (quadtree-se quadtree) nil)
  t)
