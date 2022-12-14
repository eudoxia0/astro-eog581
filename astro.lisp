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
