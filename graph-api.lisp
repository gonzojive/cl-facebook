(in-package :cl-facebook)

(defun graph-request (access-token path &key parameters)
  "
Getting an object by ID:
https://graph.facebook.com/ID

Getting relationships:
https://graph.facebook.com/ID/CONNECTION_TYPE

see http://developers.facebook.com/docs/api#search
"
  (let ((uri (format nil "https://graph.facebook.com/~A" path)))
    (json:decode-json-from-string
     (drakma:http-request uri :force-ssl t :method :get
                          :parameters `(,@parameters ("access_token" . ,access-token))))))


