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

(let ((ly (make-instance 'light-years :value 3.32)))
  (assert (= (value (light-years-to-parsecs ly)) 1.0179152)))

(let ((pc (make-instance 'parsecs :value 41.5)))
  (assert (= (value (parsecs-to-light-years pc)) 135.35474)))

;;; Conversion
