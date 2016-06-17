(in-package #:montezuma)

;; TODO: Port to flexi-streams
(defclass token-stream ()
  ())

(defgeneric next-token (token-stream)
  (:documentation "Retrieve the next token from TOKEN-STREAM."))


