
;; TL;DR:
;; - this is a simplified example to demonstrate the fundamentals of 0D
;; - for brevity, I omit handler code for Containers (which deals with connection directions)
;;     -- you need the above for useful 0D programming ; it ain't hard to add, but, the atomic principles of 0D are easier to understand if such nuance is elided


(defmacro cycle (&body body) `(cl:loop ,@body))
(defmacro exit-when (test) `(cl:when ,test (cl:return)))

(defmacro fld (%self %field)
  `(gethash ,%field ,%self))

(defmacro fset (%self %field %e)
  `(setf (gethash ,%field ,%self) ,%e))

(defmacro Object/new ()
  `(make-hash-table :test 'equal))
  

;; fundamental - Queue
(defun Queue/new ()
  (let ((self (Object/new)))
    (fset self 'queue nil)
    (fset self 'input-end nil)
    (fset self 'output-end nil)
    self))

(defun Queue/put (q v)
  (push v (fld q 'queue))
  (fset q 'input-end (fld q 'queue))
  (fset q 'output-end (last (fld q 'queue)))
  q)

(defun Queue/get (q)
  (let ((v-as-list (fld q 'output-end)))
    (when (null v-as-list) (error "Queue/get on empty queue"))
    (let ((v (car v-as-list)))
      (fset q 'queue (butlast (fld q 'queue)))
      (fset q 'output-end (last (fld q 'queue)))
      (fset q 'input-end (fld q 'queue)) ;; this will set input-end to nil when the queue is empty
      v)))

(defun Queue/empty? (q)
  (null (fld q 'queue)))

(defun Queue/length (q)
  (length (fld q 'queue)))



;; fundamental - Message
(defun Message/new (&key (port "") (payload nil))
  (let ((self (Object/new)))
    (fset self 'port port)
    (fset self 'payload payload)
    self))
(defun Message/port (self) (fld self 'port))
(defun Message/payload (self) (fld self 'payload))

;;; SWB - SoftWare Block (inspired by LEGO(R) block)

;; constructor
(defun SWB/new ()
  (let ((self (Object/new)))
    (fset self 'name "<noname>")
    (fset self 'input (Queue/new))
    (fset self 'state 'idle)
    (fset self 'handler nil)
    self))

;; accessors
(defun SWB/input (self)
  (fld self 'input))

(defun SWB/name (self)
  (fld self 'name))

(defun SWB/state (self)
  (fld self 'state))

(defun SWB/handler (self msg outq)
  (funcall (fld self 'handler) self msg outq))




  


;; fundamental - Connection
(defun Connection/new (direction from-who from-port to-who to-port)
  (let ((self (Object/new)))
    (fset self 'direction direction) ;; one of {down | up | across | through }
    (fset self 'sender from-who)
    (fset self 'send-port from-port)
    (fset self 'receiver to-who)
    (fset self 'receive-port to-port)
    self))
(defun Connection/sender (self) (fld self 'sender))
(defun Connection/send-port (self) (fld self 'send-port))
(defun Connection/receiver (self) (fld self 'receiver))
(defun Connection/receive-port (self) (fld self 'receive-port))



;; Container - a SWB which is composed recursively of SWBs
;; for the sake of brevity and lack-of-nuance of this example, I omit the message handler for Containers 
;;  (Containers need to have handlers for true 0D-ness - see less brief examples)
(defun Container/new ()
  (let ((self (SWB/new)))
    (fset self 'children nil)
    (fset self 'connections nil)
    self))
(defun Container/children (self) (fld self 'children))
(defun Container/connections (self) (fld self 'connections))

(defun Container/any-child-ready (self)
  (mapc #'(lambda (child)
	    (when (not (Queue/empty? (SWB/input child)))
	      (return-from Container/any-child-ready t)))
    (Container/children self))
  nil)
  
(defun Container/make-msg-relative-to-receiver (msg port)
  (Message/new :port port :payload (Message/payload msg)))

(defun Container/route-outputs (container-self child outq)
  (cycle
   (exit-when (Queue/empty? outq))
   (let ((out-msg (Queue/get outq)))
     (mapc #'(lambda (c)
              (when (and (eq (Connection/sender c) child) (equal (Message/port out-msg) (Connection/send-port c)))
                (let ((remapped-msg (Container/make-msg-relative-to-receiver out-msg (Connection/receive-port c))))
                  (let ((receiver (Connection/receiver c)))
                    (Queue/put (SWB/input receiver) remapped-msg)))))
           (Container/connections container-self)))))

(defun Container/dispatch-some-child (self)
  (mapc #'(lambda (child)
	    (when (not (Queue/empty? (SWB/input child)))
	      (let ((in-msg (Queue/get (SWB/input child))))
		(let ((outq (Queue/new)))
		  (SWB/handler child in-msg outq)
                  (Container/route-outputs self child outq)
                  (return-from Container/dispatch-some-child)))))
        (Container/children self)))

(defun Container/dispatcher (self)
  (cycle
   (exit-when (not (Container/any-child-ready self)))
   (Container/dispatch-some-child self)))


;;;;;;;;;;;


;; Example SWB - Reader

;;; read 1 character from stream fd, return nil on EOF
(defun getc (fd)
  (read-char fd nil nil))

	  
(defun Reader/handler (self msg outq)
  (cond
    ((eq (SWB/state self) 'idle)
      (cond
	((equal (fld msg 'port) "initialize")
	  (fset self 'filename (fld msg 'payload))
	  (fset self 'fd (open (fld self 'filename) :direction :input)))
	((equal (fld msg 'port) "request")
	  (let ((c (getc (fld self 'fd))))
	    (if (not (null c))
	      (Queue/put outq (Message/new :port "" :payload c))
	      (Queue/put outq (Message/new :port "eof" :payload "")))))
	(t (error (format nil "unhandled message in Reader ~a" msg)))))
    (t (error "unknown state for Reader"))))

(defun Reader/new ()
  (let ((self (SWB/new)))
    (fset self 'filename "")
    (fset self 'fd nil)
    (fset self 'handler #'Reader/handler)
    (fset self 'name "Reader")
    self))



;; Example SWB - Writer
(defun Writer/handler (self msg outq)
  (cond
    ((eq (SWB/state self) 'idle)
      (cond
	((equal (fld msg 'port) "go")
	  (Queue/put outq (Message/new :port "request" :payload t)))
	((equal (fld msg 'port) "")
	  (format *standard-output* "~a" (fld msg 'payload))
	  (Queue/put outq (Message/new :port "request" :payload t)))
	((equal (fld msg 'port) "eof")
	  (quit))
	(t (error (format nil "unhandled message in Writer ~a" msg)))))
    (t (error "unknown state for Writer"))))

(defun Writer/new ()
  (let ((self (SWB/new)))
    (fset self 'handler #'Writer/handler)
    (fset self 'name "Writer")
    self))



;; Example Container
(defun Example-Container/new ()
  (let ((self (Container/new)))
    (let ((reader-child (Reader/new))
	   (writer-child (Writer/new)))
    (fset self 'children (list reader-child writer-child))
    (fset self 'connections (list
			      (Connection/new 'across reader-child "" writer-child "")
			      (Connection/new 'across writer-child "request" reader-child "request")))
      self)))

(defun Example-Container/reader (self)
  (first (Container/children self)))

(defun Example-Container/writer (self)
  (second (Container/children self)))


(defun get-zd-directory () #P"~/projects/zd-in-cl/")

;;; run the example by invoking (main) from the REPL
(defun main ()
  (let ((*default-pathname-defaults* (get-zd-directory)))
    (let ((top (Example-Container/new)))
      ;; initialization
      (let ((r (Example-Container/reader top)))
        (Queue/put (fld r 'input) (Message/new :port "initialize" :payload (merge-pathnames "test.txt")))
        (Container/dispatcher top))
      ;; run
      (let ((w (Example-Container/writer top)))
        (Queue/put (fld w 'input) (Message/new :port "go" :payload t))
        (Container/dispatcher top))))
  (values))

(main)
