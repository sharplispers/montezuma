(in-package #:montezuma)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *valid-index-options*
    '(:path
      :create-if-missing-p
      :create-p
      :default-field
      :id-field
      :default-search-field
      :analyzer
      :directory
      :close-directory-p
      :occur-default
      :wild-lower-p
      :fields
      :default-slop
      :key
      :use-compound-file-p
      :handle-parse-errors-p
      :auto-flush-p
      :merge-factor
      :min-merge-docs
      :max-merge-docs
      :info-stream)))

(defun index-options-list-p (list)
  (do ((options list (cddr options)))
      ((endp options) T)
    (when (not (member (car options) *valid-index-options*))
      (return-from index-options-list-p NIL))))

(deftype index-option () `(member ,@*valid-index-options*))
(deftype index-options-list () '(satisfies index-options-list-p))

(defun get-index-option (options option &optional default)
  (check-type option index-option)
  (getf options option default))

(define-setf-expander get-index-option (place option &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (declare (ignore writer-form))
    (let ((goption (gensym "OPTION"))
	  (gstore (if store-vars (car store-vars) (gensym "STORE"))))
      (values
       (list* goption vars)
       (list* option vals)
       (list gstore)
       `(progn
	  (check-type ,goption index-option)
	  (setf (getf ,reader-form ,goption) ,(car store-vars)))
       `(getf ,goption ,reader-form)))))


(defclass index ()
  ((key)
   (dir)
   (has-writes-p :initform NIL)
   (reader :initform nil)
   (writer)
   (close-dir-p)
   (auto-flush-p)
   (default-search-field)
   (default-field)
   (analyzer :reader analyzer)
   (searcher :initform nil)
   (open-p :initform T)
   (options)
   (qp :initform nil)))
   

(defmethod initialize-instance :after ((self index) &rest args &key &allow-other-keys)
  (with-slots (options) self
    (check-type args index-options-list)
    (setf options (copy-list args))
    (setf (get-index-option options :default-field)
	  (if (get-index-option options :default-field)
	      (string (get-index-option options :default-field))
	      ""))
    (setf (get-index-option options :default-search-field)
	  (or (get-index-option options :default-search-field)
	      (get-index-option options :default-field)
	      "*"))
    (setf (get-index-option options :create-if-missing-p)
	  (get-index-option options :create-if-missing-p T))
    ;; FIXME: I don't flatten the :key option, I'm not sure why Ferret does.
    (with-slots (key dir options close-dir-p auto-flush-p create-p analyzer writer
		     default-search-field default-field) self
      (setf key (get-index-option options :key))
      (cond ((get-index-option options :path)
	     (handler-case
		 (setf dir (make-fs-directory (get-index-option options :path)
					      :create-p (get-index-option options :create-p)))
	       (error () (setf dir (make-fs-directory (get-index-option options :path)
						      :create-p (get-index-option options :create-if-missing-p)))))
	     (setf (get-index-option options :close-directory-p) T))
	    ((get-index-option options :directory)
	     (setf dir (get-index-option options :directory)))
	    (T
	     (setf (get-index-option options :create-p) T)
	     (setf dir (make-instance 'ram-directory))))
      ;; Create the index if need be
      (setf writer (apply #'make-instance 'index-writer
			  :directory dir
			  options))
      (setf (get-index-option options :analyzer) (setf analyzer (analyzer writer)))
      (close writer)
      (setf writer nil)
      ;; Only want to create the first time, if at all.
      (setf (get-index-option options :create-p) NIL)
      (setf close-dir-p (get-index-option options :close-directory-p))
      (setf (get-index-option options :close-directory-p) NIL)
      (setf auto-flush-p (get-index-option options :auto-flush-p))
      (setf default-search-field (or (get-index-option options :default-search-field)
				     (get-index-option options :default-field)
				     "*"))
      (setf default-field (or (get-index-option options :default-field) ""))
      (when (not (get-index-option options :handle-parse-errors-p))
	(setf (get-index-option options :handle-parse-errors-p) T)))))
    

(defmethod close ((self index))
  (with-slots (open-p reader writer dir) self
    (when (not open-p)
      (error "Tried to close an already closed directory."))
    (when reader (close reader))
    (when writer (close writer))
    (close dir)
    (setf open-p NIL)))

(defgeneric reader (index))

(defmethod reader ((self index))
  (ensure-reader-open self)
  (slot-value self 'reader))

(defgeneric searcher (index))

(defmethod searcher ((self index))
  (ensure-searcher-open self)
  (slot-value self 'searcher))

(defgeneric writer (index))

(defmethod writer ((self index))
  (ensure-writer-open self)
  (slot-value self 'writer))

(defmethod document-count ((self index))
  (with-slots (writer) self
    (document-count writer)))

(defgeneric add-document-to-index (index doc &optional analyzer))

(defmethod add-document-to-index ((self index) doc &optional analyzer)
  (let ((fdoc nil)
	(default-field (slot-value self 'default-field)))
    (when (listp doc)
      ;; Turn association lists into something we can treat like any
      ;; other table (including hash tables).
      (setf doc (convert-alist-to-table doc)))
    (cond ((stringp doc)
	   (setf fdoc (make-instance 'document))
	   (add-field fdoc (make-field default-field doc
				       :stored T :index :tokenized)))
	  ((typep doc 'array)
	   (setf fdoc (make-instance 'document))
	   (dosequence (field doc)
	     (add-field fdoc (make-field default-field field
					 :stored T :index :tokenized))))
	  ((table-like-p doc)
	   (setf fdoc (make-instance 'document))
	   (dolist (field (table-keys doc))
	     (let ((text (table-value doc field)))
	       (add-field fdoc (make-field (string field) (stringify text)
					  :stored T :index :tokenized)))))
	  ((typep doc 'document)
	   (setf fdoc doc))
	  (T
	   (error "Unknown document type ~S" doc)))
    ;; Delete existing documents with the same key.
    (let ((key (slot-value self 'key)))
      (when key
	(let ((query (inject key (make-instance 'boolean-query)
			     #'(lambda (query field)
				 (add-query query
					    (make-instance 'term-query
							   :term (make-term field (get-field fdoc field)))
					    :must-occur)))))
	  (query-delete self query))))
    (let ((writer (writer self)))
      (setf (slot-value self 'has-writes-p) T)
      (add-document-to-index-writer (slot-value self 'writer) fdoc
				    (if analyzer analyzer (analyzer writer)))
      (when (slot-value self 'auto-flush-p)
	(flush self)))))


;; The main search method for the index. You need to create a query to
;; pass to this method. You can also pass a hash with one or more of
;; the following; {filter, num_docs, first_doc, sort}
;;
;; query::      The query to run on the index
;; filter::     Filters docs from the search result
;; first_doc::  The index in the results of the first doc retrieved.
;;              Default is 0
;; num_docs::   The number of results returned. Default is 10
;; sort::       An array of SortFields describing how to sort the results.

(defgeneric search (index query &rest options))

(defmethod search ((self index) query &rest options)
  (do-search self query options))


(defgeneric search-each (index query fn &optional options))

(defmethod search-each ((self index) query fn &optional options)
  (let ((hits (do-search self query options)))
    (dosequence (score-doc (score-docs hits))
      (funcall fn (doc score-doc) (score score-doc)))
    (total-hits hits)))

(defmethod get-document ((self index) id)
  (let ((reader (reader self)))
    (cond ((stringp id)
	   (get-document-with-term reader (make-term "id" id)))
	  ((typep id 'term)
	   (get-document-with-term reader id))
	  (T
	   (get-document reader id)))))

(defmethod delete-document ((self index) id)
  (let ((reader (reader self)))
    (let ((count (cond ((stringp id)
			(delete-docs-with-term reader (make-term "id" id)))
		       ((typep id 'term)
			(delete-docs-with-term reader id))
		       ((integerp id)
			(delete-document reader id))
		       (T
			(error "Can't delete for id ~S" id)))))
      (when (slot-value self 'auto-flush-p)
	(flush self))
      count)))

(defgeneric query-delete (index query))

(defmethod query-delete ((self index) query)
  (let ((reader (reader self))
	(searcher (searcher self))
	(query (process-query self query)))
    (search-each searcher query
		 #'(lambda (doc score)
		     (declare (ignore score))
		     (delete-document reader doc)))
    (when (slot-value self 'auto-flush-p)
      (flush self))))

(defmethod deleted-p ((self index) n)
  (deleted-p (reader self) n))


(defgeneric update (index id new-val))

(defmethod update ((self index) id new-val)
  (with-slots (options) self
    (cond ((stringp id)
	   ;; FIXME: how about using a pre-parsed form of query?
	   (query-update self (format nil "id:~A" id) new-val))
	  ((typep id 'term)
	   (query-update self
			 (make-instance 'term-query
					:term id)
			 new-val))
	  ((integerp id)
	   (let ((reader (reader self))
		 (document (get-document self id)))
	     (when (listp new-val)
	       (setf new-val (convert-alist-to-table new-val)))
	     (cond ((table-like-p new-val)
		    (dolist (name (table-keys new-val))
		      (let ((content (table-value new-val name)))
			(setf (document-values document name) (string content)))))
		   ((typep new-val 'document)
		    (setf document new-val))
		   (T
		    (setf (document-values document (get-index-option options :default-field))
			  (string new-val))))
	     (delete-document reader id)
	     (let ((writer (writer self)))
	       (add-document-to-index-writer writer document))))
	  (T
	   (error "Cannot update for id ~S" id)))
    (when (slot-value self 'auto-flush-p)
      (flush self))))

(defgeneric query-update (index query new-val))

(defmethod query-update ((self index) query new-val)
  (let ((searcher (searcher self))
	(reader (reader self))
	(docs-to-add '())
	(query (process-query self query)))
    (search-each searcher query
		 #'(lambda (id score)
		     (declare (ignore score))
		     (let ((document (get-document self id)))
		       (when (listp new-val)
			 (setf new-val (convert-alist-to-table new-val)))
		       (cond ((table-like-p new-val)
			      (dolist (name (table-keys new-val))
				(let ((content (table-value new-val name)))
				  (setf (document-values document name) (string content)))))
			     ((typep new-val 'document)
			      (setf document new-val))
			     (T
			      (setf (document-values document (get-index-option (slot-value self 'options) :default-field))
				    (string new-val))))
		       (push document docs-to-add)
		       (delete-document reader id))))
    (let ((writer (writer self)))
      (dolist (doc (reverse docs-to-add))
	(add-document-to-index-writer writer doc))
      (when (slot-value self 'auto-flush-p)
	(flush self)))))

(defmethod has-deletions-p ((self index))
  (has-deletions-p (reader self)))

(defgeneric has-writes (index))

(defmethod has-writes ((self index))
  (slot-value self 'has-writes))

(defmethod flush ((self index))
  (with-slots (reader writer searcher) self
    (when reader (close reader))
    (when writer (close writer))
    (setf reader nil
	  writer nil
	  searcher nil)))

(defmethod optimize ((self index))
  (optimize (writer self))
  (flush self))

(defmethod size ((self index))
  (num-docs (reader self)))

(defmethod add-indexes ((self index) &rest indexes)
  (when (> (length indexes) 0)
    (when (typep (elt indexes 0) 'index)
      (setf indexes (map 'list #'reader indexes)))
    (cond ((typep (elt indexes 0) 'index-reader)
	   (let ((reader (reader self)))
	     (setf indexes (remove reader indexes)))
	   (add-indexes-readers (writer self) indexes))
	  ((typep (elt indexes 0) 'directory)
	   (setf indexes (remove (slot-value self 'dir) indexes))
	   (apply #'add-indexes (writer self) indexes))
	  (T
	   (error "Unknown index type ~S when trying to merge indexes." (elt indexes 0))))))

(defgeneric persist (index directory &key create-p))

(defmethod persist ((self index) directory &key (create-p T))
  (flush self)
  (with-slots (dir options) self
    (let ((old-dir dir))
      (etypecase directory
	((or string pathname)
	 (setf dir (make-fs-directory directory :create-p create-p))
	 (setf (get-index-option options :close-directory-p) T))
	(directory
	 (setf dir directory)))
      (ensure-writer-open self)
      (add-indexes (writer self) old-dir))))



(defgeneric ensure-writer-open (index))

(defmethod ensure-writer-open ((self index))
  (with-slots (open-p writer reader dir options searcher) self
    (unless open-p
      (error "Tried to use a closed index."))
    (unless writer
      (when reader
	(close reader)
	(setf reader nil
	      searcher nil))
      (setf writer (apply #'make-instance 'index-writer
				  :directory dir
				  options)))))

(defgeneric ensure-reader-open (index))

(defmethod ensure-reader-open ((self index))
  (with-slots (open-p writer reader dir) self
    (unless open-p
      (error "Tried to use a closed index."))
    (if reader
	(if (not (latest-p reader))
	  (setf reader (open-index-reader dir :close-directory-p NIL))
	  NIL)
	(progn
	  (when writer
	    (close writer)
	    (setf writer nil))
	  (setf reader (open-index-reader dir :close-directory-p NIL))))))

(defgeneric ensure-searcher-open (index))

(defmethod ensure-searcher-open ((self index))
  (with-slots (open-p searcher reader) self
    (unless open-p
      (error "Tried to use a closed index."))
    (when (or (ensure-reader-open self) (not searcher))
      (setf searcher (make-instance 'index-searcher
				    :reader reader)))))


(defgeneric do-search (index query options))

(defmethod do-search ((self index) query options)
  (let ((searcher (searcher self))
	(query (process-query self query)))
    (apply #'search searcher query options)))


(defgeneric process-query (index query))

(defmethod process-query ((self index) query)
  (if (stringp query)
      (with-slots (qp default-search-field options reader) self
	(unless qp
	  (setf qp (apply #'make-instance 'query-parser
			  :allow-other-keys T
			  :default-field default-search-field
			  options)))
	;; We need to set this every time, in case a new field has
	;; been added.
	(unless (get-index-option options :fields)
	  (setf (fields qp) (coerce (get-field-names reader) 'vector)))
	(parse qp query))
      query))

(defun stringify (x)
  (etypecase x
    (string x)
    ((or character symbol) (string x))
    (integer (format nil "~A" x))))
