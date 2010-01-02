(define spec (create-specification))

(define ast
  (with-specification
    spec
    (ast-rule 'Root->Config-Scheduler*<Schedulers-Scheduler<CurrentScheduler)
    (ast-rule 'Config->AbstractWorker*<Workers)

    (ast-rule 'AbstractWorker->id-state-timestamp)
    (ast-rule 'Switch:AbstractWorker->AbstractWorker*<Workers)
    (ast-rule 'Worker:AbstractWorker->devicetype-Request*<Queue)

    (ast-rule 'Request->id-size-time)

    (ast-rule 'Scheduler->name-method)

    (compile-ast-specifications 'Root)

    (ag-rule
      lookup-worker
      (Worker
        (lambda (n id)
          (if (= id (ast-child 'id n))
            n
            #f)))

      (Switch
        (lambda (n id)
          (if (= id (ast-child 'id n))
            n
            (ast-find-child*
              (lambda (i worker)
                (att-value 'lookup-worker worker id))
              (ast-child 'Workers n)))))

      (Config
        (lambda (n id)
          (if (= id 0)
            n
            (ast-find-child*
              (lambda (i worker)
                (att-value 'lookup-worker worker id))
              (ast-child 'Workers n))))))



    (compile-ag-specifications)

    (create-ast 'Root
                (list
                  (create-ast 'Config (list (create-ast-list (list))))
                  (create-ast-list (list))
                  (create-ast-bud)))))


(define config (ast-child 'Config ast))


(define add-node-to-ast
  (lambda (parent-id n)
    ((parent (att-value 'lookup-worker config parent-id))
     (workers (attr-value 'Workers parent)))
    (rewrite-add workers n)))


(define add-worker-to-ast
  (lambda (id parent-id device-type time)
    (add-note-to-ast
      parent-id
      (create-ast spec 'Worker (list id 'OFF time device-type (create-ast-list (list)))))))


(define add-switch-to-ast
  (lambda (id parent-id time)
    (add-note-to-ast
      parent-id
      (create-ast spec 'Switch (list id 'OFF time (create-ast-list (list)))))))



(define event-worker-online
  (lambda (id time)
    #f))


(define event-worker-offline
  (lambda (id time)
    #f))


(define event-worker-off
  (lambda (id time)
    #f))


(define event-work-request
  (lambda (time work-id load-size time-due)
    #f))


(define event-work-complete
  (lambda (id time work-id)
    #f))


(define display-ast
  (lambda ()
    (print-ast
      ast
      (list)
      (current-output-port))))

;;; the old stuff ;;;
;
;(define spec (create-specification))
;
;
;(define ast
;  (with-specification
;    spec
;    (ast-rule 'Config->Worker*<Workers-Switch*<Switches)
;    (ast-rule 'Worker->id-state-timestamp-switchid-devicetype-Request*<Queue)
;    (ast-rule 'Request->id-size-time)
;    (ast-rule 'Switch->id-state-timestamp)
;    (compile-ast-specifications 'Config)
;    (ag-rule
;      lookup-worker
;      (Config
;        (lambda (n id)
;          (ast-find-child
;            (lambda (i worker) (= (ast-child 'id worker) id))
;            (ast-child 'Workers n)))))
;    (compile-ag-specifications)
;    (create-ast 'Config
;                (list
;                  (create-ast-list (list))
;                  (create-ast-list (list))))))
;
;
;(define workers (ast-child 'Workers ast))
;
;
;(define switches (ast-child 'Switches ast))
;
;
;(define add-worker-to-ast
;  (lambda (id time switch-id device-type)
;    (rewrite-add
;      workers
;      (create-ast spec 'Worker (list id 'OFF time switch-id device-type (create-ast-list (list)))))))
;
;
;(define add-switch-to-ast
;  (lambda (id time)
;    (rewrite-add
;      switches
;      (create-ast spec 'Switch (list id 'OFF time)))))
;
;
;(define get-worker
;  (lambda (id)
;    (ast-find-child
;      (lambda (i worker) (= (ast-child 'id worker) id))
;      (ast-child 'Workers ast))))
;
;
;
;
;(define display-ast
;  (lambda ()
;    (print-ast
;      ast
;      (list)
;      (current-output-port))))
;
;
;
;(define status
;  (lambda ()
;    (display "id | state | since | queue-length\n")
;    (let ((time (timestamp)))
;      (ast-for-each-child
;        (lambda (i worker)
;          (display (ast-child 'id worker))
;          (display " ")
;          (display (ast-child 'state worker))
;          (display " ")
;          (display (/ (floor (* (- time (ast-child 'timestamp worker)) 100)) 100))
;          (display " ")
;          (display (ast-num-children (ast-child 'Queue worker)))
;          (newline))
;        (ast-child 'Workers ast)))))
;
;
;
;
;
;;(define round-robbin
;;  (let ((pos 0))
;;    (lambda ()
;;      (let ((running-workers (list)))
;;        (ast-for-each-child
;;          (lambda (i worker)
;;            (if (eq? (ast-child 'state worker) 'RUNNING)
;;              (set! running-workers (append running-workers (list worker)))))
;;          workers)
;;        (set! pos
;;          (if (< pos (length running-workers))
;;            (+ pos 1)
;;            1))
;;        (if (= 0 (length running-workers))
;;          #f
;;          (list-ref running-workers (- pos 1)))))))
;
;
;(define round-robbin
;  (lambda ()
;    (let
;      ((running-workers
;         (filter
;           (lambda (worker) (eq? (ast-child 'state worker) 'RUNNING))
;           (ast-children workers))))
;      (if (null? running-workers)
;        #f
;        (fold-left
;          (lambda (w1 w2)
;            (if
;              (<=
;                (ast-num-children (ast-child 'Queue w1))
;                (ast-num-children (ast-child 'Queue w2)))
;              w1
;              w2))
;          (car running-workers)
;          (cdr running-workers))))))
;
;
;(define assign-next-request
;  (lambda (worker)
;    (let ((queue (ast-child 'Queue worker)))
;      (if (not (= 0 (ast-num-children queue)))
;        (let ((request (ast-child 1 queue)))
;          (add-event
;            'event-work-command
;            (ast-child 'id worker)
;            (ast-child 'id request)
;            0
;            (ast-child 'size request)))
;        #f))))
;
;
;(define event-worker-online
;  (lambda (id time)
;    (let ((worker (get-worker id)))
;      (rewrite-terminal 'state worker 'RUNNING)
;      (rewrite-terminal 'timestamp worker time)
;      (assign-next-request worker))))
;
;
;(define event-worker-offline
;  (lambda (id time)
;    (let ((worker (get-worker id)))
;      (when (not (eq? (ast-child 'state worker) 'HALTING))
;        (begin
;          (rewrite-terminal 'state worker 'OFF)
;          (rewrite-terminal 'timestamp worker time))))))
;
;
;(define event-worker-off
;  (lambda (id time)
;    (let ((worker (get-worker id)))
;      (rewrite-terminal 'state worker 'OFF)
;      (rewrite-terminal 'timestamp worker time))))
;
;
;(define event-work-request
;  (lambda (time work-id load-size time-due)
;    (let ((worker (round-robbin)))
;      (if worker
;        (let*
;          ((queue (ast-child 'Queue worker))
;           (worker-idle? (= 0 (ast-num-children queue))))
;          (rewrite-add
;            queue
;            (create-ast spec 'Request (list work-id load-size time-due)))
;          (when worker-idle? (assign-next-request worker)))
;        (display "no worker running\n")))))
;
;
;(define event-work-complete
;  (lambda (id time work-id)
;    (let*
;      ((worker (get-worker id))
;       (queue (ast-child 'Queue worker))
;       (request
;         (ast-find-child
;           (lambda (i request) (= (ast-child 'id request) work-id))
;           queue)))
;      (if request
;        (begin
;          (rewrite-delete request)
;          (assign-next-request worker))
;        (display "no request with specified id found")))))
;
;
