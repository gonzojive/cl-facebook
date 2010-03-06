;;; (c) 2009 Matt Novenstern <mnovenstern@students.colgate.edu>
;;; (c) 2007 Red Daly
;;;
;;; Released under the BSD license.


;;;; Facebook API for the superior Common Lisp
(defpackage :cl-facebook
  (:nicknames #:fb #:facebook)
  (:use :cl)
  (:export
   ;; conditions
   #:facebook-error
   #:facebook-net-error
   #:facebook-server-error
   #:feed-action-limit-error
   ;; utility
   #:generate-signature
   #:verify-form-params
   #:decode-form-params
   #:decode-form-uids
   #:list-to-csv
   #:facebook-time-to-date
   ;; facebook-session properties
   #:facebook-auth-id
   #:session-secret
   #:api-key
   #:secret
   #:session-key
   #:uid
   #:expires
   ;; misc
   #:profile-uri
   ;; Login/session getting methods
   #:facebook-auth-id
   #:facebook-login
   #:establish-facebook-session
   #:make-session
   #:session-request
   #:facebook
   ;; Save session
   #:save-session-to-file
   #:load-session-from-file
   ;; methods for calling facebook API
   #:execute-query

   #:publish-action-of-user
   #:publish-story-to-user
   #:post-link
   #:get-links

   #:events-by-eids
   #:events-getmembers
   #:events-rsvp

   #:set-profile-markup
   #:get-profile-markup

   #:send-notification-request
   #:send-notification
   #:get-notifications

   #:name-by-uid
   #:users-getinfo

   ;; Status functions
   #:facebook-set-status
   
   ;; Permission for special actions
   #:get-permission 
   #:revoke-permission
   ;; (get-permission session) "set_status"for, e.g. "set_status" to get permission to set a user's status.
   )
  )

(in-package :cl-facebook)

;; (require 'trivial-https)
;; (require 'md5)
;; (require 'cl-json)
;; (require 'cl-ppcre)

(define-condition facebook-error (error)
  ()
  (:documentation "Any facebook error."))

(define-condition facebook-net-error (facebook-error)
  ()
  (:documentation "Error because of network transmission balderdash."))

(define-condition facebook-server-error (facebook-error)
  ((response :initarg :response :accessor error-response :initform nil)
   (message :initarg :message :accessor error-message :initform nil))
  (:documentation "Error response from the server."))

(define-condition feed-action-limit-error (facebook-server-error)
  ()
  (:documentation "Too many news feed posts have already been created about
a given user."))

(defun detect-and-raise-errors (response)
  "Response is a JSON response from the Facebook server."
  (when (and (listp response) (listp (first response))
	     (assoc :error_code response))
    (let ((code (cdr (assoc :error_code response)))
	  (message (cdr (assoc :error_msg response))))
      (case code
	(341 (error 'feed-action-limit-error :response response :message message))
	(t (error 'facebook-server-error :response response :message message))))))


;;; utilities
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))


;; this next method stolen from Araneida

(defun url-reserved-character-p (c)
  (not (or (member c '(#\- #\_ #\. #\! #\~ #\* #\' #\( #\) ))
           (alphanumericp c))))

(defun uri-escape (string)
   "Escape the string so it can be put into a URL."
   (apply #'concatenate 'string
          (loop for c across string
                if (url-reserved-character-p c)
                collect (format nil "%~2,'0X" (char-code c))
                else
                collect (string c))))

(defun format-uri-param (value &key (escape-function #'uri-escape))
  "Formats the value appropriately for presentation in a URI.  Lists
are transformed into comma-separated strings"
  (typecase value
    (string (funcall escape-function value))
    (list (format nil "~{~A~^,~}" (mapcar escape-function value)))))

(defun http-get (uri)
  (drakma:http-request uri :want-stream t :force-ssl t :method :get))

(defun vanilla-http-post (uri content &optional headers)
  "POSTS to the given URL with the given content and headers.
Returns the same values as drakma http-request."
  (let ((content-type (or (cdr (assoc "CONTENT-TYPE" headers :test #'equal))
			  "application/x-www-form-urlencoded")))
    (drakma:http-request uri
                         :want-stream t
                         :force-ssl t
                         :method :post 
                         :content-type content-type 
                         :content content)))

(defun http-post (uri params-alist &key headers); (param-rhs-processor #'identity))
  "Posts the given PARAMS-ALIST to URI with the given CONTENT-TYPE and extra headers.
PARAM-RHS-PROCESSOR is a function that transforms the RHS of each params-alist member
before it is ...

Returns the same values as drakma http-request."
  (vanilla-http-post uri (generate-post-content params-alist) headers))

(defun list-to-csv (values)
  "Create a list of comma separated values from a list"
  (format nil "~{~S~^,~}" values))


(defun generate-post-content (params-alist)
  "Generates the post content given an alist of parameters."
  (let ((content
	 (format nil "~{~A~^&~}"
		 (mapcar #'(lambda (arg-pair)
			     (format nil "~A=~A"
				     (uri-escape (car arg-pair))
				     (uri-escape (cdr arg-pair))))
			 params-alist))))
    content))

(defvar *console-output* *standard-output*)

(defun fetch-json-response-with-method (method)
  (multiple-value-bind (body status headers uri stream)
      (funcall method)
    (when (not (eql 200 status))
       ;;"Non-200 response code: ~A." (first response)))
      (error 'facebook-net-error))
    (prog1 (json:decode-json stream)
      (close stream))))

(defun fetch-json-response (uri)
  "Makes an HTTP request to the specified URI and decodes the JSON object the server
returns."
;  (let ((in-stream (third (http-get uri))))
;    (loop (yes-or-no-p "Read next line? ")
;      (format *console-output* "~A" (read-line in-stream)))))
;  (let ((str (red-util:slurp-stream (third (http-get uri)))))
;    (format *console-output* str)
;    (json:decode-json-from-string str)))
  (flet ((get-m () (http-get uri)))
    (fetch-json-response-with-method #'get-m)))

(defun fetch-json-post-response (uri content)
  "Makes an HTTP request to the specified URI and decodes the JSON object the server
returns."
  (flet ((post-m () (vanilla-http-post uri content)))
    (fetch-json-response-with-method #'post-m)))

;;; facebook constants
(define-constant +rest-server-uri+ "https://api.facebook.com/restserver.php"
  "Base URI where all requests are made to the facebook server.")
(define-constant +get-session-method+ "auth.getSession"
  "Method for establishing a connection once an auth token is obtained.
http://developers.facebook.com/documentation.php?v=1.0&method=auth.getSession")


;;; facebook-specific somewhat reusable utilities
(defun generate-signature (args-alist secret)
  "Generates a signature, as per
http://developer.facebook.com/documentation.php?v=1.0&doc=auth."
;; args = array of args to the request, formatted in arg=val pairs
;; sorted_array = alphabetically_sort_array_by_keys(args);
;; request_str = concatenate_in_order(sorted_array);
;; signature = md5(concatenate(request_str, secret))"
  (labels ((md5-sequence (thing)
	     (format nil "~(~{~2,'0X~}~)"
		     (map 'list #'identity (md5:md5sum-sequence thing)))))
    (let* ((sorted-args (sort (copy-list args-alist) #'string-lessp :key #'car))
	   (sig
	    (md5-sequence
	     (format nil "~{~A~}~A"
		     (mapcar #'(lambda (arg-pair)
				 (format nil "~A=~A"
					 (car arg-pair)
					 (format-uri-param (cdr arg-pair)
							   :escape-function #'identity)))
			     sorted-args)
		     secret))))
      sig)))

(defun prepare-post-request (facebook-method api-key secret &optional args-alist)
  "Genreates a facebook request.  Returns two values: the URI to request and
the content of the HTTP POST request."
  (let* ((prelim-args-alist
	  (append args-alist `(("method" . ,facebook-method)
			       ("api_key" . ,api-key)
			       ("v" . "1.0")
			       )))
	 (sig (generate-signature prelim-args-alist secret)))
    (let ((uri +rest-server-uri+))
      (values 
       uri
       (generate-post-content (cons (cons "sig" sig) prelim-args-alist))))))

  
  
  

(defun generate-request-uri (method api-key secret &optional args-alist)
  "Generates a thing."
  (let* ((prelim-args-alist
	 (append args-alist `(("method" . ,method)
			      ("api_key" . ,api-key)
			      ("v" . "1.0")
			      )))
	 (sig (generate-signature prelim-args-alist secret)))
    (let ((uri
	   (format nil "~A?~{~A~^&~}"
		   +rest-server-uri+
		   (mapcar #'(lambda (arg-pair)
			       (format nil "~A=~A"
				       (uri-escape (car arg-pair))
				       (uri-escape (cdr arg-pair))))
			   (cons (cons "sig" sig)
				 prelim-args-alist)))))
      uri)))

(defclass facebook-session ()
  ((api-key :accessor api-key          :initarg :api-key)
   (secret  :accessor secret           :initarg :secret)
   (call-id :accessor current-call-id                        :initform 1)
   (session-key :accessor session-key  :initarg :session-key :initform nil)
   (uid         :accessor uid          :initarg :uid         :initform nil :type number)
   (expires     :accessor expires      :initarg :expires     :initform nil)
   (session-secret :accessor session-secret :initarg :session-secret))
  (:documentation "A Facebook Session."))

(defun facebook-auth-id (api-key secret)
  "Returns the auth id needed for *everything* else"
  (multiple-value-bind (uri content)
      (prepare-post-request "auth.createToken"
			    api-key
			    secret
			    '(("format" . "JSON")))
    (fetch-json-post-response uri content)))

(defgeneric next-call-id (thing)
  (:documentation "Return the next 'call-id' value to use for a request to
the facebook server.")
  (:method ((session facebook-session))
    (incf (current-call-id session))))

(defun facebook-login (api-key auth-id)
  (sb-ext:run-program "/usr/bin/xdg-open" `(,(format nil "http://facebook.com/login.php?api_key=~A&v=1.0&auth_token=~A" api-key auth-id))))
  

(defun make-session (&key api-key secret session-key uid session-secret expires)
  (when (and api-key secret session-key uid session-secret)
    (make-instance 'facebook-session
		   :api-key api-key
		   :secret secret
		   :session-key session-key
		   :session-secret session-secret
		   :expires expires
		   :uid uid)))

(defun session-request (session method params)
  "Makes a Facebook API request with the current session and returns the decoded
JSON response (which is usually an alist)."
  (assert (session-secret session))
  (multiple-value-bind (uri post-content)
      (prepare-post-request
       method (api-key session) (session-secret session)
       `(("format" . "json")
	 ("call_id" . ,(format nil "~A" (next-call-id session)))
	 ("session_key" . ,(session-key session))
	 ,@params))
    (format t "~A?~A" uri post-content)
    (let ((response (fetch-json-post-response uri post-content)))
  ;     (format t "~A : ~A~%" response (listp response))
        (detect-and-raise-errors response)
        response)))

;;; user-level interaction with the library
(defun establish-facebook-session (api-key secret auth-token)
  "Create a facebook session.  API-KEY and SECRET are persistent values associated
with each application; AUTH-TOKEN is a string obtained from a user's HTTP request
to your application's page."
  (let* ((uri (generate-request-uri "auth.getSession" api-key secret
				`(("auth_token" . ,auth-token)
				  ("format" . "json"))))
	 (json-response (fetch-json-response uri)))
;    (print (json:decode-json (third json-response)))
    (print uri)
    (let ((session-key (cdr (assoc :session--key json-response)))
;	  (uid (parse-integer (cdr (assoc :uid json-response))))
	  (uid (cdr (assoc :uid json-response)))
	  (expires (cdr (assoc :expires json-response)))
	  (session-secret (cdr (assoc :secret json-response))))
      (when (and session-key uid expires session-secret)
	(make-instance 
	 'facebook-session
	 :api-key api-key :secret secret :session-key session-key :uid uid :session-secret session-secret :expires expires)))))

;; news feed
(defun publish-action-of-user (session title &key body images)
  "Publishes a story about a user to be displayed in the news feed section of her
profile.

TITLE: markup - The markup displayed in the feed story's title section
BODY:  markup - Optional - The markup displayed in the feed story's body section
IMAGES: a list of up to 4 images to display as part of the news feed image.  Each
        item in the list may also be a cons of an (image-url . image-link-url)."
  (session-request
   session "feed.publishUserAction"
   `(("template_data" . ,(format nil "{\"story\":\"~A\"}" title))
     ("template_bundle_id" . "61931264134")
     ,@(when body (list (cons "body" body)))
     )))
;     ,@(let ((count 1))
;	    mapcar #'(lambda (img-item)
;		   (if (consp img-item)

(defun publish-story-to-user (session title &key body images)
  "Publishes a story to a user.  Displayed in the news feed on the user's home page.

TITLE: markup - The markup displayed in the feed story's title section
BODY:  markup - Optional - The markup displayed in the feed story's body section
IMAGES: a list of up to 4 images to display as part of the news feed image.  Each
        item in the list may also be a cons of an (image-url . image-link-url)."
  (session-request
   session "facebook.feed.publishStoryToUser"
   `(("title" . ,title)
     ,@(when body (list (cons "body" body)))
     )))

;; profile
(defun set-profile-markup (session markup &optional uid)
  "Sets the FBML for a user's profile, including the content for both the profile box
and the profile actions. See the FBML documentation for a description of the markup
and its role in various contexts.

UID: int - The user whose profile FBML is to be set."
  (session-request
   session "facebook.profile.setFBML"
   `(("markup" . ,markup)
     ,@(when uid (list (cons "uid" (format nil "~A" uid))))
     )))
  
(defun get-profile-markup (session &optional uid)
  "Gets the FBML that is currently set for a user's profile.

UID: int - The user whose profile FBML is to be fetched."
  (session-request
   session "facebook.profile.getFBML"
   (when uid (list (cons "uid" uid)))))

;; facebook query language
(defun execute-query (session query-string)
  "Evaluates an FQL (Facebook Query Language) query  with the logged-in user."
  (session-request
   session "facebook.fql.query"
   `(("query" . ,query-string))))

;; friends
(defun are-friends-p (session uid-pairs)
  "UID-PAIRS is an alist of (uid1 . uid2).  Result is a list
corresponding to the uid-pairs list with T indicating that pair of
people are friends and NIL otherwise."
  (session-request
   session "facebook.friends.areFriends"
   `(("uids1" . ,(mapcar #'car uid-pairs))
     ("uids2" . ,(mapcar #'car uid-pairs)))))

;; notifications
(defun send-notification-request (session uids &key type content image-url invite-p)
  "Sends a notification request (via facebook.notifications.sendRequest)

UIDS is a list of user ids to whom to send this request

TYPE is type of request/invitation. e.g. the word 'event' in '1 event invitation.'

CONTENT Content of the request/invitation. This should be FBML containing only links
and the special tag <fb:req-choice url='' label='' /> to specify the buttons to be
included in the request.

IMAGE-URL an image to show beside the request. It will be resized to be 100 pixels wide.

INVITE-P Whether to call this an 'invitation' or a 'request'

The function returns a string URL if the user sending the notifications must confirm
that she would like to send out invitations to the given users.  Otherwise NIL
is returned to indicate that no notification must be sent."
  ;; formatted values
  (when (and uids type content image-url)
    (let ((result
	   (session-request
	    session "facebook.notifications.sendRequest"
	    `(("to_ids"  . ,(format nil "~{~A~^,~}" uids))
	      ("type"    . ,type)
	      ("content" . ,content)
	      ,@(when image-url (list `("image" .  ,image-url)))
	      ("invite"  . ,(if invite-p "1" "0"))))))
      (cond
	((stringp result)
	 (if (eql 0 (length result))
	     nil
	     result))
	(t (error "Unknown behavior: send-notification returned non-string, non-error reply"))))))

(defun send-notification (session uids &key notification-content email-content)
  "Sends a notification to a set of users.

SESSION - the session of the user on behalf of whom we send the notifications.

NOTIFICATION-CONTENT - the FBML to put on notification page.. whatever that is.

EMAIL-CONTENT - the FBML to put in the email notification for the user.

The notification and email parameters are a very stripped-down set of FBML which
allows only tags that result in just text and links, and in the email, linebreaks. 
There is one additional tag supported within the email parameter - use fb:notif-subject
 around the subject of the email."
  ;; formatted values
  (when (not (and session notification-content))
    (error "Must supply both session notification-content."))
  (when uids
    (let ((result
	   (session-request
	    session "facebook.notifications.send"
	    `(("to_ids"  . ,(format nil "~{~A~^,~}" uids))
	      ("notification" . ,notification-content)
	      ,@(when email-content (list `("email" .  ,email-content)))))))
      (cond
	((stringp result)
	 (if (eql 0 (length result))
	     nil
	     result))
	(t (error "Unknown behavior: send-notification returned non-string, non-error reply"))))))
  
(defun get-notifications (session)
  "Gets a list of notifications pending for the current user.
 Format of the result is the following:
 ((:MESSAGES
   (:UNREAD . 0) (:MOST_RECENT . 1184178277))
 (:POKES 
  (:UNREAD . 1) (:MOST_RECENT . 1184447424))
 (:SHARES
  (:UNREAD . 0) (:MOST_RECENT . 0))
 (:FRIEND_REQUESTS 500031322 514127705 208906)
 (:GROUP_INVITES)
 (:EVENT_INVITES))"
  ;; formatted values
  (let ((result (session-request session "notifications.get" nil)))
    result))


(defun verify-form-params (params-alist secret)
  "When a form is posted to your web site from facebook, this verifies that the parameters
are legitimate."
  (let* ((sig (cdr (assoc "fb_sig" params-alist :test #'equal)))
	 (sig-verify-params
	  (remove-if 
	    #'null
	    (mapcar #'(lambda (param-pair)
			(cl-ppcre:register-groups-bind (sig-param-name)
			    ("fb_sig(?:_)?(.*)" (car param-pair))
			  (cond 
			    ((equal "" sig-param-name) nil)
			    ((not (null sig-param-name)) 
			     (cons sig-param-name (cdr param-pair)))
			    (t param-pair))))
		    params-alist)))
	 (actual-sig
	  (generate-signature sig-verify-params secret)))
;    (format *the-output* "~A~%" sig-verify-params)
    (equal sig actual-sig)))

(defun decode-form-params (params-alist secret)
  "Verifies and decodes a request from a facebook server.  PARAMS-ALIST is (param-string . value)
and secret is the secret key provided by facebook.

Returns 4 values: 
1. T if valid, NIL if invalid
IF VALID:
2. profile uid
3. person who clicked uid
4. session key of person who clicked
5. API key of the application
TODO 6. time of submission (universally encoded)

WAS GOING TO RETURN:
1. user whose profile form was submitted from
2. user who submitted
3. time the submission occured"
  (labels ((param (str &optional type)
	     (let ((result (cdr (assoc str params-alist :test #'equal))))
	       (case type
		 (:number (and result (parse-integer result)))
		 (t result)))))
    (if (not (verify-form-params params-alist secret))
	nil
	(let ((profile-uid (param "fb_sig_profile" :number))
	      (user-uid    (param "fb_sig_user" :number))
	      (session-key (param "fb_sig_session_key"))
	      (api-key     (param "fb_sig_api_key"))
	      (time        (param "fb_sig_time")))
	  (values t profile-uid user-uid session-key api-key time)))))

(defun decode-form-uids (params-alist)
  (labels ((param (i)
	     (let ((result (cdr (assoc (format nil "ids[~A]" i) 
				       params-alist :test #'equal))))
	       (and result (parse-integer result)))))
    (do* ((i 0 (+ 1 i))
	  (id (param i) (param i))
	  (result (if id (list id) nil) (if id (cons id result) result)))
	 ((null id) (nreverse result)))))

;;; other functionality
(defun profile-uri (uid)
  "Returns a string URI for the user's profile."
  (format nil "http://www.facebook.com/profile.php?id=~A" uid))

(defun facebook-set-status (session status)
  (session-request session "status.set" `(("status" . ,status))))

(defun get-permission (session permission)
  "Needs to be run once to get user to allow the program to change their status.
 Values:

 email -  This permission allows an application to send email to its user. Send emails with notifications.sendEmail or directly to the proxied_email FQL field.

offline_access - This permission grants an application access to user data when the user is offline or doesn't have an active session.

status_update - This extended permission grants access to the users.setStatus method.

photo_upload - This permission relaxes requirements on the photos.upload and photos.addTag methods. If the user grants this permission, photos uploaded by the application will bypass the pending state and the user will not have to manually approve the photos each time.

create_listing - This permission allows an app to create and modify listings for a user via the marketplace.removeListing and marketplace.createListing methods. The user will not need to approve each listing individually.

create_event - This permission allows an app to create and modify events for a user via the events.create, events.edit and events.cancel methods.

rsvp_event - This permission allows an app to RSVP to an event on behalf of a user via the events.rsvp method.

sms - This permission allows a mobile application to send messages to the user and respond to messages from the user via text message.

video_upload - This permission allows an application to provide the mechanism for a user to upload videos to their profile.

create_note - This permission allows an application to provide the mechanism for a user to write, edit, and delete notes on their profile.

share_item - This permission allows an application to provide the mechanism for a user to post links to their profile. "
  (sb-ext:run-program "/usr/bin/xdg-open" `(,(format nil "http://www.facebook.com/authorize.php?api_key=~A&v=1.0&ext_perm=~A" (api-key session) permission))))

(defun revoke-permission (session permission)
  "Revokes permissions for the application - see permissions in get-permission"
  (if (stringp permission)
      (session-request session "auth.revokeExtendedPermission" `(("perm" . ,permission)))))

;; Information lookup functions

(defun name-by-uid (session uid)
  (cdr (assoc :name (car (session-request session "fql.query" 
		   `(("query" . ,(format nil "SELECT name FROM user WHERE uid = \"~A\"" uid))))))))

(defun get-status (session)
  (cdr (assoc :message (car (session-request session "status.get" '())))))

(defun events-by-eids (session eids)
  "Get event info by event id"
  (session-request session "events.get" `(("eids" . ,(list-to-csv eids)))))

(defun events-rsvp (session eid rsvp)
  "rsvp to an event the application is admin of"
  (if (and session (stringp rsvp) (integerp eid))
      (session-request session "events.rsvp" `(("eid" . ,eid)
					       ("rsvp_status" . ,rsvp)))))

(defun events-getmembers (session eid)
  "Get people attending event with id 'eid'"
  (if (and session (integerp eid))
      (session-request session "events.getMembers" `(("eid" . ,(format nil "~A" eid))))))

(defun post-link (session url comment)
  "Post a link to user's profile.  Requires the share_item permission"
  (if (and (stringp url) (stringp comment))
      (session-request session "links.post" `(("url" . ,url)
					      ("comment" . ,comment)))
      nil))

(defun get-links (session)
  "Lists links posted by the application to the user's pages"
  (session-request session "links.get" `()))

(defun users-getinfo (session uids fields)
  "Returns info about 'uids' containing 'fields'"
  (if (and session (listp uids) (listp fields))
      (session-request session "users.getInfo" `(("uids" . ,(format nil "~{~A~^,~}" uids))
("fields" . ,(format nil "~{~A~^,~}" fields))))))

(defun get-assoc-value (alist name)
	   (cdr (assoc name alist)))

(defun facebook-time-to-date (unix-time)
  (decode-universal-time (+ unix-time (encode-universal-time 0 0 0 1 1 1970))))

(defun save-session-to-file (session file-path &optional if-does-not-exist)
  (let ((savefile (open file-path :direction :output :if-exists :supersede :if-does-not-exist (or if-does-not-exist nil)))
	(params `(,(cons :uid (uid session))
		   ,(cons :session-key (session-key session))
		   ,(cons :session-secret (session-secret session))
		   ,(cons :api-key (api-key session))
		   ,(cons :expires (expires session))
		   ,(cons :secret (secret session)))))
    (print params)
    (when savefile (write params :stream savefile))
    (if savefile
	(close savefile)
	nil)))

(defun load-session-from-file (file-path &optional if-does-not-exist)
  (let* ((loadfile (open file-path :direction :input :if-does-not-exist (or if-does-not-exist nil)))
	 (alist (read loadfile nil))
	 (api-key (get-assoc-value alist :api-key))
	 (expires (get-assoc-value alist :expires))
	 (secret (get-assoc-value alist :secret))
	 (uid (get-assoc-value alist :uid))
	 (session-secret (get-assoc-value alist :session-secret))
	 (session-key (get-assoc-value alist :session-key)))
    (print alist)
    (when loadfile (close loadfile)
    (make-session :api-key api-key
		  :secret secret
		  :session-key session-key
		  :session-secret session-secret
		  :expires expires
		  :uid uid))))