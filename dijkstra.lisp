;;;;
;;;; An implementation of Dijkstra's algorithm in Common Lisp. While this
;;;; implementation is generic (i.e. works for any graph), the purpose is to
;;;; feed it a graph where the nodes are stars, and two nodes `(A,B)` are linked
;;;; by an edge if `d(A,B) < C`, where `C` is an arbitrary cutoff distance. The
;;;; cost of each edge is the distance.
;;;;

(defclass edge ()
  ((start :reader edge-start
          :initarg :start
          :type integer
          :documentation "The ID of the start node.")
   (end :reader edge-end
        :initarg :end
        :type integer
        :documentation "The ID of the end node.")
   (cost :reader edge-cost
         :initarg :cost
         :type number
         :documentation "The cost of traversing this edge."))
  (:documentation "Represents an edge in a graph."))

(defmethod initialize-instance :after ((edge edge) &key)
  (with-slots (start end cost) edge
    ;; Check edges are not degenerate.
    (when (= start end)
      (error "Degenerate edge: start and end IDs are the same: ~A" start))
    ;; Check cost is non-negative.
    (when (< cost 0.0)
      (error "Edge cost is negative: ~A" cost))))

(defclass graph ()
  ((vertices :accessor graph-vertices
             :initarg :vertices
             :type (vector integer)
             :documentation "A vector of vertex IDs.")
   (edges :accessor graph-edges
          :initarg :edges
          :type (vector edge)
          :documentation "A vector of edge objects."))
  (:documentation "Represents a graph."))

;(declaim (ftype (function ((vector edge)) graph) make-graph-from-edges))
(defun make-graph-from-edges (edges)
  "Construct a graph from a vector of edges."
  (let ((vertex-table (make-hash-table :test 'equal)) ; table of seen IDs
        (vertices (make-array 0 :adjustable t :element-type 'integer :fill-pointer 0))) ; vertex accumulator
    (loop for edge across edges do
      (with-slots (start end) edge
        (unless (gethash start vertex-table)
          (vector-push-extend start vertices)
          (setf (gethash start vertex-table) t))
        (unless (gethash end vertex-table)
          (vector-push-extend end vertices)
          (setf (gethash end vertex-table) t))))
    (make-instance 'graph :vertices vertices
                          :edges edges)))

;(declaim (ftype (function ((vector star) parsecs) graph) make-graph))
(defun make-graph (stars dist)
  "Create a graph from a vector of stars. Only create edges between stars that are less than DIST parsecs apart."
  (let ((n (length stars)) ; Number of stars.
        (edges (make-array 0 :adjustable t :element-type 'edge :fill-pointer 0))) ; edge accumulator
    ;; Do the Cartesian product.
    (loop for i from 0 to (- n 1) do
      (loop for j from (+ i 1) to (- n 1) do
        (let ((a (elt stars i))
              (b (elt stars j)))
          (assert (not (= (star-id a) (star-id b))))
          ;; Find the distance between the two stars.
          (let ((d (euclidean-distance (star-cartesian-position a)
                                       (star-cartesian-position b))))
            ;; If the distance is within the radius, add the edge to the graph.
            (when (<= (value d) (value dist))
              (vector-push-extend (make-instance 'edge
                                                 :start (star-id a)
                                                 :end (star-id b)
                                                 :cost (value d))
                                  edges))))))
    ;; Construct the graph from the edges.
    (make-graph-from-edges edges)))

(defun build-path (destination previous)
  (let ((path (make-array 0 :adjustable t :element-type 'integer :fill-pointer 0)))
    (let ((u destination))
      (loop while (gethash u previous) do
        (vector-push-extend u path)
        (setf u (gethash u previous)))
      (vector-push-extend u path)
      (reverse path))))

(defun make-neighbors (graph)
  "Construct the neighbors map. This is a hash table from vertex IDs to a hash
  table of vertex IDs to costs. That is:

Map[ID, Map[ID, Float]]"
  (with-slots (vertices edges) graph
    (let ((neighbors (make-hash-table :test 'equal)))
      ;; Initialize.
      (loop for vertex across vertices do
        (setf (gethash vertex neighbors) (make-hash-table :test 'equal)))
      ;; Fill.
      (loop for edge across edges do
        (with-slots (start end cost) edge
          (setf (gethash end (gethash start neighbors)) cost)
          (setf (gethash start (gethash end neighbors)) cost)))
      ;; Return
      neighbors)))

(defun dijkstra (graph source destination)
  "Find a path from the source node to the destination in the given graph.

GRAPH is an instance of GRAPH. SOURCE and DESTINATION are integer vertex IDs.

Returns a vector of integer vertex IDs."
  ;; Table of distances.
  (let ((dist (make-hash-table :test 'equal)))
    (loop for vertex across (graph-vertices graph) do
      (setf (gethash vertex dist) double-float-positive-infinity))
    (setf (gethash source dist) 0.0)
    ;; Table of previous nodes.
    (let ((previous (make-hash-table :test 'equal)))
      (loop for vertex across (graph-vertices graph) do
        (setf (gethash vertex previous) nil))
      ;; Table of neighbor costs.
      (let ((neighbors (make-neighbors graph)))
        ;; Queue.
        (let ((q (make-hash-table :test 'equal)))
          (loop for vertex across (graph-vertices graph) do
            (setf (gethash vertex q) t))
          (labels ((pop-min ()
                     (let ((min-id nil)
                           (min-dist nil))
                       (loop for vertex being the hash-keys of q do
                         (when (or (null min-id)
                                   (< (gethash vertex dist) min-dist))
                           (setf min-id vertex)
                           (setf min-dist (gethash vertex dist))))
                       (assert (not (null min-id)))
                       (assert (not (null min-dist)))
                       (remhash min-id q)
                       min-id)))
            (loop while (> (hash-table-count q) 0) do
              (let ((u (pop-min)))
                (when (or (= u destination)
                          (= (gethash u dist) double-float-positive-infinity))
                  (return))
                ;; Adjust neighbor distances.
                (let ((neighbors (gethash u neighbors)))
                  (loop for v being the hash-keys of neighbors do
                    (let ((cost (gethash v neighbors)))
                      (let ((alt (+ cost (gethash u dist))))
                        (when (< alt (gethash v dist))
                          (setf (gethash v dist) alt)
                          (setf (gethash v previous) u))))))))
            ;; Build path
            (build-path destination previous)))))))
