(in-package #:montezuma)


(defclass test-query-parser (query-parser)
  ()
  (:default-initargs
   :analyzer (make-instance 'standard-analyzer :stop-words nil)))


(defmethod $add-and-clause ((parser test-query-parser) clauses clause)
  (setf clauses (if (listp (car clauses)) clauses (list clauses)))
  (setf clauses (remove nil clauses))
  (when (= (length clauses) 1)
    (let ((last-clause (first clauses)))
      (let ((occur (second last-clause)))
	(when (not (eq occur :must-not-occur))
	  (setf (second last-clause) :must-occur)))))
  (if clause
      (progn
	(unless (eq (second clause) :must-not-occur)
	  (setf (second clause) :must-occur))
	(cons clause clauses))
      clauses))

(defmethod $add-or-clause :around ((parser test-query-parser) clauses clause)
  (if (listp (car clauses))
      (call-next-method)
      (call-next-method parser (list clauses) clause)))

(defmethod $get-term-query ((parser test-query-parser) word)
  (let* ((field (use-active-field parser))
	 (tokens (all-tokens (slot-value parser 'analyzer) field word)))
    (cond ((= (length tokens) 0)
	   (list :term-query field ""))
	  ((= (length tokens) 1)
	   (list :term-query field (term-text (car tokens))))
	  (T
	   (list :phrase-query field (mapcar #'(lambda (token)
						 (cons (term-text token) (token-increment token)))
					     tokens))))))

(defmethod $get-boolean-clause ((parser test-query-parser) query occur)
  (list :boolean-clause occur query))

(defmethod $get-boolean-query ((parser test-query-parser) clauses)
  (setf clauses (if (listp (car clauses)) clauses (list clauses)))
  (list :boolean-query (reverse clauses)))

(defmethod $add-word-to-phrase ((parser test-query-parser) phrase word)
  (append (if (listp phrase) phrase (list phrase)) (list word)))

(defmethod get-normal-phrase-query ((parser test-query-parser) words)
  (let ((pos-inc 0)
	(field (use-active-field parser)))
    (list :phrase-query field
	  (mapcan
	   #'(lambda (word)
	       (if (null word)
		   (progn (incf pos-inc) '())
		   (let ((tokens (all-tokens (slot-value parser 'analyzer) field word)))
		     (mapcar #'(lambda (token)
				 (prog1 (cons (term-text token) (+ pos-inc (token-increment token)))
				   (setf pos-inc 0)))
			     tokens))))
	   words))))

(defmethod $get-wild-query ((parser test-query-parser) word)
  (list :wild-query (use-active-field parser) word))

(defmethod get-bad-parse ((parser test-query-parser) query-string)
  (let* ((field (use-active-field parser))
	 (tokens (all-tokens (analyzer parser) field query-string)))
    (cond ((= (length tokens) 0)
	   (list :term-query field ""))
	  ((= (length tokens) 1)
	   (list :term-query field (term-text (elt tokens 0))))
	  (T
	   (list :boolean-query (mapcar #'(lambda (token)
					    (list :boolean-clause :should-occur
						  (list :term-query field (term-text token))))
					tokens))))))


(defun check-query-parse (query-string expected-parse-tree)
  (atest check-query-parse
	 (parse (make-instance 'test-query-parser
			       :fields '("f1" "f2"))
		query-string)
	 expected-parse-tree
	 #'(lambda (a b) (tree-equal a b :test #'equal))
	 (format T "~&Query string was ~S" query-string))
  (atest check-real-query-parse
	 (parse (make-instance 'query-parser)
		query-string)
	 'query
	 #'typep
	 (format T "~&Query string was ~S" query-string)))

(deftestfun query-parser
    (let ((tests
	   '(("abc"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc")))) )
	     ("abc def"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "def"))))) 
	     ("john's"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "john")))))
	     ("john's email is jjwiseman@yahoo.com mail-to"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "john"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "email"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "is"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "jjwiseman@yahoo.com"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR
				 (:PHRASE-QUERY "*" (("mail" . 1) ("to" . 1)))))))
	     ("+abc" (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "*" "abc")))))
	     ("abc +def ghi"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "*" "def"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "ghi"))))) 
	     ("foo*"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo*")))) )
	     ("foo?bar"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo?bar")))) )
	     ("*foo*"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo*")))) )
	     ("*foo"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo")))) )
	     ("*foo*bar"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo*bar")))) )
	     ("+*foo"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :MUST-OCCUR (:WILD-QUERY "*" "*foo")))) )
	     ("!abc"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "abc")))) )
	     ("abc !def"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "def")))))
	     ("\"abc\""
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc")))))
	     ("\"abc def\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" (("abc" . 1) ("def" . 1)))))))
	     ("abc \"def hij\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" (("def" . 1) ("hij" . 1))))))) 
	     ("foo* \"bad dog\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo*"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" (("bad" . 1) ("dog" . 1))))))) 
	     ("field:value"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "field" "value")))))
	     ("field:foo*"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "field" "foo*")))))
	     ("field:*foo"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "field" "*foo")))))
	     ("+field:abc"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "field" "abc")))))
	     ("field:\"1 2 3\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "field" (("1" . 1) ("2" . 1) ("3" . 1)))))))
	     ("!\"ha ha\" !\"ha ha\" !\"ha ha\" \"ha ha\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:PHRASE-QUERY "*" (("ha" . 1) ("ha" . 1))))
		(:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:PHRASE-QUERY "*" (("ha" . 1) ("ha" . 1))))
		(:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:PHRASE-QUERY "*" (("ha" . 1) ("ha" . 1))))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" (("ha" . 1) ("ha" . 1)))))))
	     ;; The point of this test is to trigger query-parser's
	     ;; get-bad-parse method by giving it a string that
	     ;; doesn't parse.  At the moment (revision 331) this
	     ;; string won't parse, but it probably will in the
	     ;; future.
	     ("  blah:blah  "
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "blah"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "blah"))))))))
      (dolist (test tests)
	(check-query-parse (first test) (second test)))))

	    
