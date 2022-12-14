;;;;
;;;; A tiny framework for astronomy in Common Lisp.
;;;;
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

(defun hms-to-decimal (hms)
  "Convert an HMS (hours-minutes-seconds) angle to decimal degrees."
  (with-slots (hours minutes seconds) hms
    (let ((d (+ (* 15.0 hours)
                (/ (* 15.0 minutes) 60.0)
                (/ (* 15.0 seconds) 3600.0))))
      (make-instance 'decimal-degrees :value d))))

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

;;; Conversion

(defun light-years-to-parsecs (ly)
  "Convert the given distance in light years to parsecs."
  (make-instance 'parsecs :value (* (value ly) 0.306601)))

(defun parsecs-to-light-years (pc)
  "Convert the given distance in parsecs to light years."
  (make-instance 'light-years :value (* (value pc) 3.26156)))

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

;;;; Star data.

(defclass star ()
  ((id :reader star-id
       :initarg :id
       :type integer
       :documentation "The star's ID in the HYG database.")
   (proper :reader star-proper
           :initarg :proper
           :type (or null string)
           :documentation "The star's proper name, if known.")
   (hip :reader star-hip
        :initarg :hip
        :type (or null string)
        :documentation "The star's name in the Hipparcos catalog, if known.")
   (hd :reader star-hd
       :initarg :hd
       :type (or null string)
       :documentation "The star's name in the Henry Draper catalog, if known.")
   (gliese :reader star-gliese
        :initarg :gliese
        :type (or null string)
           :documentation "The star's name in the the third edition of the Gliese Catalog of Nearby Stars, if known.")
   (bayer :reader star-bayer
          :initarg :bayer
          :type (or null string)
          :documentation "The star's Bayer / Flamsteed designation, if known.")
   (distance :reader star-distance
             :initarg :distance
             :type number)
   (equatorial-position :reader star-equatorial-position
                        :initarg :equatorial-position
                        :documentation "The star's equatorial (RA, DEC, DIST) position.")
   (cartesian-position :reader star-cartesian-position
                       :initarg :cartesian-position
                       :documentation "The star's Cartesian (X, Y, Z) position."))
  (:documentation "Represents a star from the HYG database."))

(defun star-name (star)
  "A star's name. The following are tried in order: proper name, Bayer
designation, Gliese name, HIP name, HD name. If the star doesn't have any names,
returns NIL."
  (with-slots (proper bayer gliese hip) star
    (or proper
        bayer
        gliese
        (if hip
            (concatenate 'string "HIP " hip)
            "?"))))

(defmethod print-object ((star star) stream)
  (print-unreadable-object (star stream :type t)
    (format stream "~A" (star-name star))))

;;;; HYG database.

(defclass hyg-database ()
  ((stars :reader database-stars
          :initarg :stars
          :type (vector star)
          :documentation "The star vector."))
  (:documentation "The in-memory HYG database."))

(defun star-count (db)
  (length (database-stars db)))

;;; Parsing the HYG database.

(defun string-or-nil (str)
  (if (string= str "") nil str))

(defun parse-star (cells)
  (destructuring-bind (id hip hd hr gliese bayer proper ra dec dist prma prdec rv mag absmag spect ci x y z &rest etc) cells
    (declare (ignore hr ra dec prma prdec rv mag absmag spect ci etc))
    (make-instance 'star
                   :id (parse-integer id)
                   :proper (string-or-nil proper)
                   :hip (string-or-nil hip)
                   :hd (string-or-nil hd)
                   :gliese (string-or-nil gliese)
                   :bayer (string-or-nil bayer)
                   :distance (read-from-string dist)
                   :cartesian-position (make-instance 'cartesian-position
                                                      ;; FIXME: parse-number
                                                      :x (make-instance 'parsecs :value (read-from-string x))
                                                      :y (make-instance 'parsecs :value (read-from-string y))
                                                      :z (make-instance 'parsecs :value (read-from-string z))))))

(defun load-hyg-database (pathname)
  "Load the HYG database from a CSV file."
  (with-open-file (stream pathname :direction :input)
    ;; Discard the header.
    (read-line stream nil)
    (let ((stars (make-array 0 :adjustable t :element-type 'star :fill-pointer 0)))
      (loop for line = (read-line stream nil)
            while line
            do
               (let ((columns (uiop:split-string line :separator ",")))
                 (let ((star (parse-star columns)))
                   (vector-push-extend star stars))))
      (make-instance 'hyg-database :stars stars))))

;;; Finding stars

(defun find-star-by-name (db name)
  (loop for star across (database-stars db) do
    (when (string= name (star-name star))
      (return-from find-star-by-name star)))
  nil)

(defun find-star-by-id (db id)
  (loop for star across (database-stars db) do
    (when (= id (star-id star))
      (return-from find-star-by-id star)))
  nil)

;;;; Distance

(defun euclidean-distance (p1 p2)
  "Calculate the Euclidean distance between two Cartesian coordinates."
  (with-slots ((x1 x) (y1 y) (z1 z)) p1
    (with-slots ((x2 x) (y2 y) (z2 z)) p2
      (let ((x1 (value x1))
            (y1 (value y1))
            (z1 (value z1))
            (x2 (value x2))
            (y2 (value y2))
            (z2 (value z2)))
        (make-instance 'parsecs
                       :value (sqrt (+ (expt (- x1 x2) 2)
                                       (expt (- y1 y2) 2)
                                       (expt (- z1 z2) 2))))))))

(defun distance-stars (a b)
  (euclidean-distance (star-cartesian-position a) (star-cartesian-position b)))

(defun distance-from-sol (star)
  (euclidean-distance (make-instance 'cartesian-position
                                     :x (make-instance 'parsecs :value 0.0)
                                     :y (make-instance 'parsecs :value 0.0)
                                     :z (make-instance 'parsecs :value 0.0))
                      (star-cartesian-position star)))

(declaim (ftype (function (hyg-database cartesian-position parsecs) (vector star)) find-stars-within-radius))
(defun find-stars-within-radius (db pos radius)
  "Given an HYG database, a position in Cartesian coordinates, and a radius in
parsecs, return a vector of all the stars that are within the radius from that
position."
  (let ((stars (make-array 0 :adjustable t :element-type 'star :fill-pointer 0)))
    (loop for star across (database-stars db) do
      (let ((star-pos (star-cartesian-position star)))
        (when (< (value (euclidean-distance pos star-pos))
                 (value radius))
          (vector-push-extend star stars))))
    stars))
