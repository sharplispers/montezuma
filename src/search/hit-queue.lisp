(in-package #:montezuma)

(defclass hit-queue (priority-queue)
  ())

(defmethod less-than ((queue hit-queue) hit1 hit2)
  (if (= (score hit1) (score hit2))
      (> (doc hit1) (doc hit2))
      (< (score hit1) (score hit2))))

(defclass field-sorted-hit-queue (hit-queue)
  ((reader :initform nil :initarg :reader)
   (fields :initform nil :initarg :fields)))

(defmethod less-than ((queue field-sorted-hit-queue) hit1 hit2)
  (with-slots (fields reader) queue
    (flet ((hit-document-value (hit value)
             (let ((field (document-field 
                            (get-document reader (doc hit))
                            value)))
               (and 
                 field 
                 (field-data field)))))
      (loop 
        for i across fields 
        do 
        (return-from less-than 
          (with-slots (reverse-p name) i
            (let ((value-1 (hit-document-value hit1 name))
                  (value-2 (hit-document-value hit2 name)))
              (or 
                (not value-1)
                (funcall 
                  (if reverse-p 
                    #'string<
                    #'string>)
                  value-1 value-2)))))))))
