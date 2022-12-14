(require 'uiop)

;;;; Printing

(defgeneric humanize (object stream)
  (:documentation "Return a human-readable representation of an object."))

;;;; Angles

;;; Decimal

(defclass decimal-degrees ()
  ((value :reader value
          :initarg :value
          :type real))
  (:documentation "Represents an angle in decimal degrees."))

(defmethod initialize-instance :after ((d decimal-degrees) &key)
  (let ((d (value d)))
    (unless (and (> d -180.0) (<= d 180.0))
      (error "Value out of range for decimal degrees: ~A" d))))

(defmethod humanize ((angle decimal-degrees) stream)
  (format stream "~0,1f°" (value angle)))

(defmethod print-object ((angle decimal-degrees) stream)
  (print-unreadable-object (angle stream :type t)
    (humanize angle stream)))

(let ((a (make-instance 'decimal-degrees :value 49.3)))
  (assert (string= (princ-to-string a) "#<DECIMAL-DEGREES 49.3°>")))

;;; Hours-minutes-seconds (right ascension)

(defclass hms-degrees ()
  ((hours :reader hours
          :initarg :hours
          :type real)
   (minutes :reader minutes
            :initarg :minutes
            :type real)
   (seconds :reader seconds
            :initarg :seconds
            :type real))
  (:documentation "Represents an angle in HMS (hours-minutes-seconds) format."))

(defmethod initialize-instance :after ((angle hms-degrees) &key)
  (with-slots (hours minutes seconds) angle
    (unless (and (>= hours 0.0) (< hours 24.0))
      (error "Invalid value for hours in hms-degrees: ~a" hours))
    (unless (and (>= minutes 0.0) (< minutes 60.0))
      (error "Invalid value for minutes in hms-degrees: ~a" minutes))
    (unless (and (>= seconds 0.0) (< seconds 60.0))
      (error "Invalid value for seconds in hms-degrees: ~a" seconds))))

(defmethod humanize ((angle hms-degrees) stream)
  (with-slots (hours minutes seconds) angle
    (format stream "~0,1fh~0,1fm~0,1fs" hours minutes seconds)))

(defmethod print-object ((angle hms-degrees) stream)
  (print-unreadable-object (angle stream :type t)
    (humanize angle stream)))

(let ((a (make-instance 'hms-degrees :hours 7.2 :minutes 2.24 :seconds 1.42)))
  (assert (string= (princ-to-string a) "#<HMS-DEGREES 7.2h2.2m1.4s>")))

(defun hms-to-decimal (hms)
  "Convert an HMS (hours-minutes-seconds) angle to decimal degrees."
  (with-slots (hours minutes seconds) hms
    (let ((d (+ (* 15.0 hours)
                (/ (* 15.0 minutes) 60.0)
                (/ (* 15.0 seconds) 3600.0))))
      (make-instance 'decimal-degrees :value d))))

(let ((hms (make-instance 'hms-degrees
                          :hours 8.3
                          :minutes 45.7
                          :seconds 2.3)))
  (let ((dec (hms-to-decimal hms)))
    (assert (= (value dec) 135.93459))))

;;; Degrees-minutes-seconds (declination)

(defclass dms-degrees ()
  ((degrees :reader degrees
            :initarg :degrees
            :type real)
   (minutes :reader minutes
            :initarg :minutes
            :type real)
   (seconds :reader seconds
            :initarg :seconds
            :type real))
  (:documentation "Represents an angle in DMS (degrees-minutes-seconds) format."))

(defmethod initialize-instance :after ((angle dms-degrees) &key)
  (with-slots (degrees minutes seconds) angle
    (unless (and (> degrees -90.0) (<= degrees 90.0))
      (error "Invalid value for degrees in hms-degrees: ~a" degrees))
    (unless (and (>= minutes 0.0) (< minutes 60.0))
      (error "Invalid value for minutes in hms-degrees: ~a" minutes))
    (unless (and (>= seconds 0.0) (< seconds 60.0))
      (error "Invalid value for seconds in hms-degrees: ~a" seconds))))

(defmethod humanize ((angle dms-degrees) stream)
  (with-slots (degrees minutes seconds) angle
    (format stream "~0,1f°~0,1fm~0,1fs" degrees minutes seconds)))

(defmethod print-object ((angle dms-degrees) stream)
  (print-unreadable-object (angle stream :type t)
    (humanize angle stream)))

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

(defclass light-years ()
  ((value :reader value
          :initarg :value
          :type real
          :documentation "The underlying value."))
  (:documentation "Represents distance in light years."))

(defmethod humanize ((d light-years) stream)
  (let ((d (value d)))
    (format stream "~0,1fly" d)))

(defmethod print-object ((d light-years) stream)
  (print-unreadable-object (d stream :type t)
    (humanize d stream)))

(let ((a (make-instance 'light-years :value 23.4)))
  (assert (string= (princ-to-string a) "#<LIGHT-YEARS 23.4ly>")))

(defclass parsecs ()
  ((value :reader value
          :initarg :value
          :type real
          :documentation "The underlying value."))
  (:documentation "Represents distance in parsecs."))

(defmethod humanize ((d parsecs) stream)
  (let ((d (value d)))
    (format stream "~0,1fpc" d)))

(defmethod print-object ((d parsecs) stream)
  (print-unreadable-object (d stream :type t)
    (humanize d stream)))

(let ((a (make-instance 'parsecs :value 41.1)))
  (assert (string= (princ-to-string a) "#<PARSECS 41.1pc>")))

;;; Conversion

(defun light-years-to-parsecs (ly)
  "Convert the given distance in light years to parsecs."
  (make-instance 'parsecs :value (* (value ly) 0.306601)))

(let ((ly (make-instance 'light-years :value 3.32)))
  (assert (= (value (light-years-to-parsecs ly)) 1.0179152)))

(defun parsecs-to-light-years (pc)
  "Convert the given distance in parsecs to light years."
  (make-instance 'light-years :value (* (value pc) 3.26156)))

(let ((pc (make-instance 'parsecs :value 41.5)))
  (assert (= (value (parsecs-to-light-years pc)) 135.35474)))

;;;; Star coordinates

;;; Equatorial coordinates

(defclass equatorial-position ()
  ((right-ascension :reader right-ascension
                    :initarg :right-ascension
                    :type hms-degrees
                    :documentation "The right ascension in HMS.")
   (declination :reader declination
                :initarg :declination
                :type dms-degrees
                :documentation "The declination in DMS.")
   (distance :reader distance
             :initarg :distance
             :type parsecs
             :documentation "The distance in parsecs."))
  (:documentation "A position in equatorial (RA, DEC, DIST) coordinates."))

(defmethod print-object ((p equatorial-position) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (right-ascension declination distance) p
      (write-string "RA=" stream)
      (humanize right-ascension stream)
      (write-string " DEC=" stream)
      (humanize declination stream)
      (write-string " D=" stream)
      (humanize distance stream))))

(let ((ra (make-instance 'hms-degrees :hours 23 :minutes 2.24 :seconds 1.42))
      (dec (make-instance 'dms-degrees :degrees -19.6 :minutes 45.7 :seconds 2.3))
      (d (make-instance 'parsecs :value 62.1)))
  (let ((ep (make-instance 'equatorial-position :right-ascension ra :declination dec :distance d)))
    (assert (string= (princ-to-string ep)
                     "#<EQUATORIAL-POSITION RA=23.0h2.2m1.4s DEC=-19.6°45.7m2.3s D=62.1pc>"))))

(defclass cartesian-position ()
  ((x :reader x
      :initarg :x
      :type parsecs
      :documentation "The X coordinate in parsecs.")
   (y :reader y
      :initarg :y
      :type parsecs
      :documentation "The Y coordinate in parsecs.")
   (z :reader z
      :initarg :z
      :type parsecs
      :documentation "The Z coordinate in parsecs."))
  (:documentation "A position in Cartesian (X, Y, Z) coordinates."))

(defmethod print-object ((p cartesian-position) stream)
  (print-unreadable-object (p stream :type t)
    (with-slots (x y z) p
      (write-string "X=" stream)
      (humanize x stream)
      (write-string " Y=" stream)
      (humanize y stream)
      (write-string " Z=" stream)
      (humanize z stream))))

(let ((pos (make-instance 'cartesian-position
                          :x (make-instance 'parsecs :value 12.3)
                          :y (make-instance 'parsecs :value 45.6)
                          :z (make-instance 'parsecs :value 7.8))))
  (assert (string= (princ-to-string pos) "#<CARTESIAN-POSITION X=12.3pc Y=45.6pc Z=7.8pc>")))

;;; Equatorial-to-cartesian conversion.
;;; See: http://www.projectrho.com/public_html/starmaps/trigonometry.php

(defun rad (x)
  (* x 0.0174532925))

(defun sinr (x) (sin (rad x)))
(defun cosr (x) (cos (rad x)))

(defun equatorial-to-cartesian (pos)
  "Convert a position from equatorial to cartesian coordinates."
  (with-slots (right-ascension declination distance) pos
    (let ((φ (value (hms-to-decimal right-ascension)))
          (θ (value (dms-to-decimal declination)))
          (ρ (value distance)))
      (let ((rvect (* ρ (cosr θ))))
        (let ((x (* rvect (cosr φ)))
              (y (* rvect (sinr φ)))
              (z (* ρ     (sinr θ))))
          (make-instance 'cartesian-position
                         :x (make-instance 'parsecs :value x)
                         :y (make-instance 'parsecs :value y)
                         :z (make-instance 'parsecs :value z)))))))

(let ((ra (make-instance 'hms-degrees :hours 1 :minutes 41 :seconds 45))
      (dec (make-instance 'dms-degrees :degrees -16.0 :minutes 12.0 :seconds 0.0))
      (d (make-instance 'parsecs :value 3.61)))
  (let ((tau-ceti (make-instance 'equatorial-position :right-ascension ra :declination dec :distance d)))
    (assert (string= (princ-to-string tau-ceti) "#<EQUATORIAL-POSITION RA=1.0h41.0m45.0s DEC=-16.0°12.0m.0s D=3.6pc>"))
    (with-slots (x y z) (equatorial-to-cartesian tau-ceti)
      (assert (= (value x) 3.1305823))
      (assert (= (value y) 1.4890215))
      (assert (= (value z) -1.0071579)))))
