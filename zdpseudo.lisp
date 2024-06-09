
// TL;DR:
// - this is a simplified example to demonstrate the fundamentals of 0D
// - for brevity, I omit handler code for Containers (which deals with connection directions)
//     -- you need the above for useful 0D programming ; it is not hard to add, but, the atomic principles of 0D are easier to understand if such nuance is elided


  

// fundamental - Queue

defobj Queue {
    fresh () {
	.queue = nil
	.input-side = nil
	.output-side = nil
    }
    
    method put (v) {
	push (v .queue)
	.input-side = .queue
	.output-side = last (.queue)
    }

    method get () {
	synonym v-as-list = .output-side {
	    when null? (v-as-list) { error "Queue/get on empty queue" }
            synonym v = car (v-as-list) {
		.queue = butlast (.queue)
		.output-end = last (.queue)
		.input-end = .queue
		v
	    }
	}
    }

    method empty? () { null? (.queue) }

    method length () { length (.queue) }
}

// fundamental - Message
defobj Message {
    fresh (port payload) {
	.port = port
	.payload = payload
    }
    method port () { .port }
    method payload () {	.payload }
}


//; SWB - SoftWare Block (inspired by LEGO(R) block)

// constructor
defobj SWB {
    fresh () {
	.name = "<noname>"
	.input = Queue/new ()
	.state = 'idle'
        .handler = ϕ
    }

    method input () { .input }
    method name () { .name }
    method state () { .state }
    method handler (msg outq) {	funcall (.handler msg outq) }
}


  
// fundamental - Connection
defobj Connection {
    fresh (direction from-who from-port to-who to-port) {
	.direction = direction
	.sender = from-who
	.sender-port = from-port
	.receiver = to-who
	.receive-port = to-port
    }

    method sender () { .sender }
    method send-port () { .send-port }
    method receiver () { .receiver }
    method receive-port () { .receive-port }
}





// Container - a SWB which is composed recursively of SWBs
// for the sake of brevity and lack-of-nuance of this example, I omit the message handler for Containers 
//  (Containers need to have handlers for true 0D-ness - see less brief examples)
defobj Container {
    fresh () {
        .children = ⎣⎦
	.connections = ⎣⎦
    }
    method children () { .children }
    method connections () { .connections }
    method any-child-ready () {
	∀ child in .children {
	    when not child.queue.empty? () {✅}
	}
	✗
    }
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


//////////;


// Example SWB - Reader

//; read 1 character from stream fd, return nil on EOF
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



// Example SWB - Writer
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



// Example Container
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

//; run the example by invoking (main) from the REPL
(defun main ()
  (let ((*default-pathname-defaults* (get-zd-directory)))
    (let ((top (Example-Container/new)))
      // initialization
      (let ((r (Example-Container/reader top)))
        (Queue/put (fld r 'input) (Message/new :port "initialize" :payload (merge-pathnames "test.txt")))
        (Container/dispatcher top))
      // run
      (let ((w (Example-Container/writer top)))
        (Queue/put (fld w 'input) (Message/new :port "go" :payload t))
        (Container/dispatcher top))))
  (values))

(main)
