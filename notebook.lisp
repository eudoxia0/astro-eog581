;;;;
;;;; The calculations specific to the story.
;;;;

;;; Load the HYG database.

(defparameter +db+
  (load-hyg-database #p"hygdata_v3.csv"))

(assert (= (star-count +db+) 119614))

;;; Find the relevant stars.

(defparameter +sol+ (find-star-by-name +db+ "Sol"))
(defparameter +g581+ (find-star-by-name +db+ "Gl 581"))
(defparameter +g555+ (find-star-by-name +db+ "Gl 555"))
(defparameter +bpic+ (find-star-by-name +db+ "Bet Pic"))

;;; Find the star closest to Gliese 581.

(let ((stars (find-stars-within-radius +db+
                                       (star-cartesian-position +g581+)
                                       (make-instance 'parsecs :value 3.0))))
  ;; Sort stars by distance.
  (flet ((dist (star)
           ;; Distance to Gliese 581.
           (star-euclidean-distance +g581+ star)))
    (let ((sorted (sort stars
                        #'(lambda (star-a star-b)
                            (< (value (dist star-a))
                               (value (dist star-b)))))))
      ;; Print the list of stars.
      (format t "Ten stars closest to Gliese 581:~%~%")
      (format t " DIST    NAME~%")
      (format t "---------------~%")
      ; subseq because the first star is Gl581 itself, and because we only want the top 10
      (loop for star across (subseq sorted 1 11) do
        (format t "~0,2fly   ~A~%"
                (value (parsecs-to-light-years (dist star)))
                (star-name star)))
      (let ((star (elt sorted 1)))
        (format t "~%The star closest to Gliese 581 is ~A at ~0,2fly"
                (star-name star)
                (value (parsecs-to-light-years (dist star))))))))

(format t "~%~%")

;;; Calculate the network route from Ctesiphon (Beta Pictoris) to Wepwawet
;;; (Gliese 555).
;;;
;;; To begin, we have to pare down the search space. There's over 100k stars in
;;; the HYG database, and we only care about the ones within roughly ~70ly
;;; (~22pc) of the Sun, since that's how far Beta Pictoris is.

(defparameter +stars+
  (remove-if #'(lambda (star)
                 (> (value (star-distance star)) 22.0))
             (copy-seq (database-stars +db+))))

(format t "There are ~A stars within ~,1fly of the Sun.~%~%"
        (length +stars+)
        (value (parsecs-to-light-years (make-instance 'parsecs :value 22.0))))

;;; Interstellar communications lasers have a range of 5pc (~16ly). This number
;;; is completely arbitrary and probably too high.

(defparameter +laser-limit+
  (make-instance 'parsecs :value 5.0))

;;; Now we construct a graph where the stars are nodes, every pair of stars is
;;; connected by an edge if they are within communications laser distance.

(defparameter +graph+
  (make-graph +stars+ +laser-limit+))

(format t "Star graph has ~A vertices and ~A edges.~%~%"
        (length (graph-vertices +graph+))
        (length (graph-edges +graph+)))

(defparameter +path+
  (loop for id across (dijkstra +graph+ (star-id +bpic+) (star-id +g581+))
        collecting (find id (database-stars +db+) :key #'star-id)))

(format t "Network route has ~A jumps.~%~%" (1- (length +path+)))

;;; Print the network route.

(format t "~12@A ~12@A ~10@A~%" "Start" "End" "Dist")
(format t "------------------------------------~%")
(loop for (a b) on +path+ by #'cdr while b do
  (format t "~12@A ~12@A ~8,2fly~%"
          (star-name a)
          (star-name b)
          (value (star-euclidean-distance a b))))

(format t "~%")

(format t "Distance from Beta Pictoris to Gliese 555: ~,2fly~%~%"
        (value (parsecs-to-light-years (star-euclidean-distance +bpic+ +g555+))))

(let ((length 0.0))
  (loop for (a b) on +path+ by #'cdr while b do
    (incf length (value (star-euclidean-distance a b))))
  (format t "Total network route length: ~,2fly~%"
          (value (parsecs-to-light-years (make-instance 'parsecs :value length)))))
