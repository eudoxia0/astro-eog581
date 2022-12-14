;;;;
;;;; Tests of the astronomy framework.
;;;;

;;;; Angles

;;; Decimal

(let ((a (make-instance 'decimal-degrees :value 49.3)))
  (assert (string= (princ-to-string a) "#<DECIMAL-DEGREES 49.3°>")))

;;; HMs

(let ((a (make-instance 'hms-degrees :hours 7.2 :minutes 2.24 :seconds 1.42)))
  (assert (string= (princ-to-string a) "#<HMS-DEGREES 7.2h2.2m1.4s>")))

(let ((hms (make-instance 'hms-degrees
                          :hours 8.3
                          :minutes 45.7
                          :seconds 2.3)))
  (let ((dec (hms-to-decimal hms)))
    (assert (= (value dec) 135.93459))))

;;; DMS

(let ((a (make-instance 'dms-degrees :degrees 51.2 :minutes 2.24 :seconds 1.42)))
  (assert (string= (princ-to-string a) "#<DMS-DEGREES 51.2°2.2m1.4s>")))

(defun sign (x)
  (cond ((< x 0.0)
         -1)
        ((= x 0.0)
         0)
        (t
         1.0)))

(defun dms-to-decimal (dms)
  "Convert a DMS (degrees-minutes-seconds) angle to decimal degrees."
  (with-slots (degrees minutes seconds) dms
    (let ((d (* (sign degrees)
                (+ (abs degrees)
                   (/ minutes 60.0)
                   (/ seconds 3600.0)))))
      (make-instance 'decimal-degrees :value d))))

(let ((dms (make-instance 'dms-degrees
                          :degrees -19.6
                          :minutes 45.7
                          :seconds 2.3)))
  (let ((dec (dms-to-decimal dms)))
    (assert (= (value dec) -20.362307))))

;;;; Distances

(let ((a (make-instance 'light-years :value 23.4)))
  (assert (string= (princ-to-string a) "#<LIGHT-YEARS 23.4ly>")))

(let ((a (make-instance 'parsecs :value 41.1)))
  (assert (string= (princ-to-string a) "#<PARSECS 41.1pc>")))

;;; Conversion

(let ((ly (make-instance 'light-years :value 3.32)))
  (assert (= (value (light-years-to-parsecs ly)) 1.0179152)))

(let ((pc (make-instance 'parsecs :value 41.5)))
  (assert (= (value (parsecs-to-light-years pc)) 135.35474)))

;;;; Star coordinates

;;; Equatorial coordinates

(let ((ra (make-instance 'hms-degrees :hours 23 :minutes 2.24 :seconds 1.42))
      (dec (make-instance 'dms-degrees :degrees -19.6 :minutes 45.7 :seconds 2.3))
      (d (make-instance 'parsecs :value 62.1)))
  (let ((ep (make-instance 'equatorial-position :right-ascension ra :declination dec :distance d)))
    (assert (string= (princ-to-string ep)
                     "#<EQUATORIAL-POSITION RA=23.0h2.2m1.4s DEC=-19.6°45.7m2.3s D=62.1pc>"))))

;;; Cartesian coordinates

(let ((pos (make-instance 'cartesian-position
                          :x (make-instance 'parsecs :value 12.3)
                          :y (make-instance 'parsecs :value 45.6)
                          :z (make-instance 'parsecs :value 7.8))))
  (assert (string= (princ-to-string pos) "#<CARTESIAN-POSITION X=12.3pc Y=45.6pc Z=7.8pc>")))


;;; Equatorial-to-cartesian

(let ((ra (make-instance 'hms-degrees :hours 1 :minutes 41 :seconds 45))
      (dec (make-instance 'dms-degrees :degrees -16.0 :minutes 12.0 :seconds 0.0))
      (d (make-instance 'parsecs :value 3.61)))
  (let ((tau-ceti (make-instance 'equatorial-position :right-ascension ra :declination dec :distance d)))
    (assert (string= (princ-to-string tau-ceti) "#<EQUATORIAL-POSITION RA=1.0h41.0m45.0s DEC=-16.0°12.0m.0s D=3.6pc>"))
    (with-slots (x y z) (equatorial-to-cartesian tau-ceti)
      (assert (= (value x) 3.1305823))
      (assert (= (value y) 1.4890215))
      (assert (= (value z) -1.0071579)))))
