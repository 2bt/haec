(define spec (create-specification))


(define ast
  (with-specification
    spec
    (ast-rule 'Config->Worker*<Workers-Switch*<Switches)
    (ast-rule 'Worker->id-state-timestamp-switchid-Request*<Queue)
    (ast-rule 'Request->id-size-time)
    (ast-rule 'Switch->id-state-timestamp)
    (compile-ast-specifications 'Config)
    (ag-rule
      lookup-worker
      (Config
        (lambda (n id)
          (ast-find-child
            (lambda (i worker) (= (ast-child 'id worker) id))
            (ast-child 'Workers n)))))
    (compile-ag-specifications)
    (create-ast 'Config
                (list
                  (create-ast-list (list))
                  (create-ast-list (list))))))


(define workers (ast-child 'Workers ast))


(define switches (ast-child 'Switches ast))


(define add-worker-to-ast
  (lambda (id time switch-id)
    (rewrite-add
      workers
      (create-ast spec 'Worker (list id 'OFF time switch-id (create-ast-list (list)))))))


(define add-switch-to-ast
  (lambda (id time)
    (rewrite-add
      switches
      (create-ast spec 'Switch (list id 'OFF time)))))


(define get-worker
  (lambda (id)
    (ast-find-child
      (lambda (i worker) (= (ast-child 'id worker) id))
      (ast-child 'Workers ast))))




(define display-ast
  (lambda ()
    (print-ast
      ast
      (list)
      (current-output-port))))



(define status
  (lambda ()
    (display "id | state | since | queue-length\n")
    (let ((time (timestamp)))
      (ast-for-each-child
        (lambda (i worker)
          (display (ast-child 'id worker))
          (display " ")
          (display (ast-child 'state worker))
          (display " ")
          (display (/ (floor (* (- time (ast-child 'timestamp worker)) 100)) 100))
          (display " ")
          (display (ast-num-children (ast-child 'Queue worker)))
          (newline))
        (ast-child 'Workers ast)))))



(define event-worker-online
  (lambda (id time)
    (let ((worker (get-worker id)))
      (rewrite-terminal 'state worker 'RUNNING)
      (rewrite-terminal 'timestamp worker time))))


(define event-worker-off
  (lambda (id time)
    (let ((worker (get-worker id)))
      (rewrite-terminal 'state worker 'OFF)
      (rewrite-terminal 'timestamp worker time))))


(define round-robbin
  (let ((pos 0))
    (lambda ()
      (let ((running-workers (list)))
        (ast-for-each-child
          (lambda (i worker)
            (if (eq? (ast-child 'state worker) 'RUNNING)
              (set! running-workers (append running-workers (list worker)))))
          workers)
        (set! pos
          (if (< pos (length running-workers))
            (+ pos 1)
            1))
        (if (= 0 (length running-workers))
          #f
          (list-ref running-workers (- pos 1)))))))


(define assign-next-request
  (lambda (worker)
    (let ((queue (ast-child 'Queue worker)))
      (if (not (= 0 (ast-num-children queue)))
        (let ((request (ast-child 1 queue)))
          (add-event
            'event-work-assign
            (ast-child 'id worker)
            (ast-child 'id request)
            1
            (ast-child 'size request)))
        #f))))



(define event-work-request
  (lambda (time work-id load-size time-due)
    (let ((worker (round-robbin)))
      (if worker
        (let*
          ((queue (ast-child 'Queue worker))
           (worker-idle? (= 0 (ast-num-children queue))))
          (rewrite-add
            queue
            (create-ast spec 'Request (list work-id load-size time-due)))
          (when worker-idle?
            (add-event 'event-work-assign (ast-child 'id worker) work-id 1 load-size)))
        (display "no worker running\n")))))


(define event-work-complete
  (lambda (id time work-id)
    (let*
      ((worker (get-worker id))
       (queue (ast-child 'Queue worker))
       (request
         (ast-find-child
           (lambda (i request) (= (ast-child 'id request) work-id))
           queue)))
      (if request
        (begin
          (rewrite-delete request)
          (assign-next-request worker))
        (display "no request with specified id found")))))


