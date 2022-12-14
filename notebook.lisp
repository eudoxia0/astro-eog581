;;;;
;;;; The calculations specific to the story.
;;;;


;;; Load the HYG database.

(defparameter +db+
  (load-hyg-database #p"/home/eudoxia/common-lisp/astro-eog581/hygdata_v3.csv"))

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
                (star-name star))))))

(format t "~%~%")
