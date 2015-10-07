(in-package #:montezuma)

(defclass sort-type ()
  ((name :initform nil :initarg :name)
   (parser :initarg :parser)
   (comparator :initform nil :reader comparator :initarg :comparator)
   (reverse-p :initform nil :reader reverse-p))
  (:default-initargs
   :parser #'string))

(defun make-sort-type (name &optional (parser nil parser-supplied-p))
  (if parser-supplied-p
      (make-instance 'sort-type
		     :name name
		     :parser parser)
      (make-instance 'sort-type
		     :name name)))
  
(defparameter *score-sorter* (make-sort-type "score"))
(defparameter *doc-sorter* (make-sort-type "doc"))
(defparameter *auto-sorter* (make-sort-type "auto"))
(defparameter *string-sorter* (make-sort-type "string"))
(defparameter *int-sorter* (make-sort-type "int" #'parse-integer))
(defparameter *float-sorter* (make-sort-type "float" #'parse-float))


(defclass sort-field ()
  ((name :initarg :name)
   (sort-type :initarg :sort-type)
   (reverse-p :initarg :reverse-p)
   (comparator :initarg :comparator))
  (:default-initargs
   :sort-type *auto-sorter*
    :reverse-p NIL))

(defmethod initialize-instance :after ((self sort-field) &key)
  (with-slots (name sort-type) self
    (setf name (string name))
    (unless (slot-boundp self 'comparator)
      (setf (slot-value self 'comparator) (comparator (slot-value self 'sort-type))))
    (when (and (null name)
	       (not (eq sort-type *doc-sorter*))
	       (not (eq sort-type *score-sorter*)))
      (error "You must supply a field name for your sort field."))))


(defparameter *field-score* (make-instance 'sort-field
					   :name nil
					   :sort-type *score-sorter*))

(defparameter *field-doc* (make-instance 'sort-field
					 :name nil
					 :sort-type *doc-sorter*))


