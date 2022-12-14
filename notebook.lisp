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
        (format t "~%~%The star closest to Gliese 581 is ~A at ~0,3fgly"
                (star-name star)
                (value (parsecs-to-light-years (dist star))))))))

(format t "~%~%")

;;; Calculate the network routine from Ctesiphon (Beta Pictoris) to Wepwawet
;;; (Gliese 555).
;;;
;;; To begin, we have to pare down the search space. There's over 100k stars in
;;; the HYG database, and we only care about the ones within roughly ~70ly
;;; (~22pc) of the Sun, since that's how far Beta Pictoris is.

(defparameter +stars+
  (remove-if #'(lambda (star)
                 (> (value (star-distance star)) 22.0))
             (copy-seq (database-stars +db+))))

(format t "There are ~A stars within ~,1fly of the Sun.~%"
        (length +stars+)
        (value (parsecs-to-light-years (make-instance 'parsecs :value 22.0))))
