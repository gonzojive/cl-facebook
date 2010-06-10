(in-package :cl-facebook)

(defparameter *http-request-function* #'drakma:http-request)

(defun graph-request (access-token path &key uri parameters (method :get))
  "
Getting an object by ID:
https://graph.facebook.com/ID

Getting relationships:
https://graph.facebook.com/ID/CONNECTION_TYPE

see http://developers.facebook.com/docs/api#search
"
  (let ((uri (or uri
                 (format nil "https://graph.facebook.com/~A" path))))
    (graph-request-helper uri `(,@parameters ("access_token" . ,access-token))
                          :method method)))

(defun old-api-request (access-token method &key parameters (http-method :get))
  "
calls an old api method METHOD using the new https means of calling stuff

https://api.facebook.com/method/<method>?<params>
"
  (let ((uri (format nil "https://api.facebook.com/method/~A" method)))
    (graph-request-helper uri `(,@parameters ("access_token" . ,access-token) ("format" . "JSON"))
                          :method http-method)))

(defun graph-request-helper (uri parameters  &key (method :get))
  (let ((drakma:*text-content-types* (cons (cons "application" "json")
                                           drakma:*text-content-types*)))
    (json:decode-json-from-string
     (funcall *http-request-function* uri
              :method method
              :parameters parameters))))

(defun graph-search (access-token query &key type)
  (graph-request access-token "search"
                 :parameters `(("q" . ,query)
                               ("type" . ,(string-downcase (string type))))))


(defun graph-search-page (result-alist &optional (nextp t))
  (graph-request-helper (cdr (assoc (if nextp :next :previous)
                                    (cdr (assoc :paging result-alist))))
                        nil))
