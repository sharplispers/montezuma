(in-package #:montezuma)

(deftestfun test-standard-analyzer
  (with-input-from-string (input "D.Ba_l-n@gma-l.com AB&Sons Toys'r'us you're she's, #$%^$%*& job@dot I.B.M. the an AnD THEIR")
    (let* ((analyzer (make-instance 'standard-analyzer :stop-words nil))
	   (token-stream (token-stream analyzer "field" input)))
      (test standard-analyzer-1 (next-token token-stream) (make-token "d.ba_l-n@gma-l.com" 0 18) #'token=)
      (test standard-analyzer-2 (next-token token-stream) (make-token "ab&sons" 19 26) #'token=)
      (test standard-analyzer-3 (next-token token-stream) (make-token "toys'r'us" 27 36) #'token=)
      (test standard-analyzer-4 (next-token token-stream) (make-token "you're" 37 43) #'token=)
      (test standard-analyzer-5 (next-token token-stream) (make-token "she" 44 49) #'token=)
      (test standard-analyzer-6 (next-token token-stream) (make-token "job@dot" 60 67) #'token=)
      (test standard-analyzer-7 (next-token token-stream) (make-token "ibm" 68 74) #'token=)
      (test standard-analyzer-8 (next-token token-stream) (make-token "the" 75 78) #'token=)
      (test standard-analyzer-9 (next-token token-stream) (make-token "an" 79 81) #'token=)
      (test standard-analyzer-10 (next-token token-stream) (make-token "and" 82 85) #'token=)
      (test standard-analyzer-11 (next-token token-stream) (make-token "their" 86 91) #'token=)
      (test standard-analyzer-12 (next-token token-stream) nil))))

