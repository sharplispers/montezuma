(in-package #:montezuma)


(defun string-scanner (regexp string)
  (let ((start 0))
    #'(lambda ()
	(multiple-value-bind (match-start match-end)
	    (cl-ppcre:scan regexp string :start start)
	  (if (null match-start)
	      nil
	      (progn
		(setq start (if (= match-start match-end)
				(1+ match-end)
				match-end))
		(values (subseq string match-start match-end) match-start match-end)))))))
	

(defclass tokenizer (token-stream)
  ((input :initarg :input)))

(defmethod close ((self tokenizer))
  (with-slots (input) self
    (when (streamp input)
      (cl:close input))))



(defclass regexp-tokenizer (tokenizer)
  (string-scanner))

(defmethod initialize-instance :after ((self regexp-tokenizer) &key)
  (with-slots (string-scanner input) self
    (let ((input-string (if (streamp input) (stream-contents input) input)))
      (setf string-scanner (string-scanner (token-regexp self) input-string)))))

(defmethod next-token ((self regexp-tokenizer))
  (multiple-value-bind (term start end)
      (funcall (slot-value self 'string-scanner))
    (if term
	(make-token (normalize self term) start end)
	nil)))

(defgeneric token-regexp (tokenizer))

(defmethod token-regexp ((self regexp-tokenizer))
  (cl-ppcre:create-scanner "\\w+" :multi-line-mode T))

(defgeneric normalize (tokenizer string))

(defmethod normalize ((self regexp-tokenizer) str)
  str)

(defclass letter-tokenizer (regexp-tokenizer)
  ())

(defmethod token-regexp ((self letter-tokenizer))
  (cl-ppcre:create-scanner "[a-zA-Z]+" :multi-line-mode T))

(defclass lowercase-tokenizer (letter-tokenizer)
  ())

(defmethod normalize ((self lowercase-tokenizer) str)
  (string-downcase str))


(defclass whitespace-tokenizer (regexp-tokenizer)
  ())

(defmethod token-regexp ((self whitespace-tokenizer))
  (cl-ppcre:create-scanner "\\S+" :multi-line-mode T))
  
