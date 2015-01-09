; TODO:
; requests brauchen ein attribut zur zeitlichen einordnung
; requests brauchen ein child time-dispatch
;
;


(define predict-processing-timespan
  (lambda (load-size devicetype)
    (* load-size
       (cond
         ((eq? devicetype 'CUBIEBOARD) 4)
         ((eq? devicetype 'SAMA5D3) 2)))))


(define spec (create-specification))

(define ast
  (with-specification
    spec
    (ast-rule 'Root->Config-scheduler)
    (ast-rule 'Config->AbstractWorker*<Workers)

    (ast-rule 'AbstractWorker->id-state-timestamp)
    (ast-rule 'Switch:AbstractWorker->AbstractWorker*<Workers)
    (ast-rule 'Worker:AbstractWorker->devicetype-Request*<Queue)

    (ast-rule 'Request->id-size-deadline)

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

    (ag-rule
      get-running-workers
      (Worker
        (lambda (n)
          (if (eq? (ast-child 'state n) 'RUNNING)
            (list n)
            (list))))
      (Switch
        (lambda (n)
          (fold-left
            (lambda (a b)
              (append a (att-value 'get-running-workers b)))
            (list)
            (ast-children (ast-child 'Workers n)))))
      (Config
        (lambda (n)
          (fold-left
            (lambda (a b)
              (append a (att-value 'get-running-workers b)))
            (list)
            (ast-children (ast-child 'Workers n))))))


    (ag-rule
      schedule
      (Root
        (lambda (n time work-id load-size time-due)
          (let ((scheduler (ast-child 'scheduler n)))
            (cond
              ((eq? scheduler 'round-robbin)
               (let ((running-workers (att-value 'get-running-workers (ast-child 'Config n))))
                 (if (null? running-workers)
                   (values #f #f)
                   (let*
                     ((worker
                        (fold-left
                          (lambda (w1 w2)
                            (if (<=
                                  (ast-num-children (ast-child 'Queue w1))
                                  (ast-num-children (ast-child 'Queue w2)))
                              w1
                              w2))
                          (car running-workers)
                          (cdr running-workers)))
                      (index
                        (+ 1 (ast-num-children (ast-child 'Queue worker)))))
                     (values worker index)))))
              ((eq? scheduler 'smart-batmana)
               (values #f #f)))))))



    (ag-rule
      timespan-processing
      (Request
        (lambda (n devicetype)
          (predict-processing-timespan (ast-child 'size n) devicetype))))



    (ag-rule
      schedule-batman
      (Worker
        (lambda (n time work-id load-size time-due)
          ; hint: worker must be in state RUNNING
          (let*
            ((devicetype (ast-child 'devicetype n))
             (timespan-processing (predict-processing-timespan load-size devicetype))
             (queue (ast-child 'Queue n))
             (index
               (let next ((index (ast-num-children queue)))
                 (cond
                   ((= index 0) 1)
                   ((> time-due
                       (ast-child 'deadline (ast-child index queue)))
                    (+ index 1))
                   ((= index 1) #f)
                   (#t (next (- index 1)))




    (compile-ag-specifications)

    (create-ast
      'Root
      (list
        (create-ast 'Config (list (create-ast-list (list))))
        'round-robbin))))


(define display-ast
  (lambda ()
    (print-ast
      ast
      (list)
      (current-output-port))))


(define config (ast-child 'Config ast))


(define add-node-to-ast
  (lambda (parent-id n)
    (let* ((parent (att-value 'lookup-worker config parent-id))
           (workers (ast-child 'Workers parent)))
      (rewrite-add workers n))))


(define add-worker-to-ast
  (lambda (id parent-id device-type time)
    (add-node-to-ast
      parent-id
      (create-ast spec 'Worker (list id 'OFF time device-type (create-ast-list (list)))))))


(define add-switch-to-ast
  (lambda (id parent-id time)
    (add-node-to-ast
      parent-id
      (create-ast spec 'Switch (list id 'OFF time (create-ast-list (list)))))))


(define dispatch-next-request
  (lambda (worker)
    (let ((queue (ast-child 'Queue worker)))
      (if (not (= 0 (ast-num-children queue)))
        (let ((request (ast-child 1 queue)))
          (add-event
            'event-work-command
            (ast-child 'id worker)
            (ast-child 'id request)
            0
            (ast-child 'size request)))
        #f))))


(define event-worker-online
  (lambda (id time)
    (let ((worker (att-value 'lookup-worker config id)))
      (rewrite-terminal 'state worker 'RUNNING)
      (rewrite-terminal 'timestamp worker time)
      (dispatch-next-request worker))))


(define event-worker-offline
  (lambda (id time)
    (let ((worker (att-value 'lookup-worker config id)))
      (when (not (eq? (ast-child 'state worker) 'HALTING))
        (begin
          (rewrite-terminal 'state worker 'ERROR)
          (rewrite-terminal 'timestamp worker time))))))


(define event-worker-off
  (lambda (id time)
    (let ((worker (att-value 'lookup-worker config id)))
      (rewrite-terminal 'state worker 'OFF)
      (rewrite-terminal 'timestamp worker time))))


(define event-work-request
  (lambda (time work-id load-size time-due)
    (let-values (((worker index) (att-value 'schedule config time work-id load-size time-due)))
      (if worker
        (let*
          ((queue (ast-child 'Queue worker))
           (worker-idle? (= 0 (ast-num-children queue))))
          (rewrite-insert
            queue
            index
            (create-ast spec 'Request (list work-id load-size time-due)))
          (when worker-idle? (dispatch-next-request worker)))
        (display "no worker running\n")))))


(define event-work-complete
  (lambda (id time work-id)
    (let* ((worker (att-value 'lookup-worker config id))
           (queue (ast-child 'Queue worker))
           (request
             (ast-find-child
               (lambda (i request) (= (ast-child 'id request) work-id))
               queue)))
      (if request
        (begin
          (rewrite-delete request)
          (dispatch-next-request worker))
        (display "no request with specified id found\n")))))


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
;(define dispatch-next-request
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
;      (dispatch-next-request worker))))
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
;          (when worker-idle? (dispatch-next-request worker)))
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
;          (dispatch-next-request worker))
;        (display "no request with specified id found\n")))))
;
;
