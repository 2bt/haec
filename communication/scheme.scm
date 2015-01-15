; TODO:
; C:
; - keep state.txt
; - implement current accumulator
; - event reset-accumulator
; - event set-scheduler
; Web:
; - select for scheduler
; - select for scenario
; - play button
; - reset button
; - stromzähler
; Scheme:
; - return error message from scheduler


(define predict-processing-timespan
  (lambda (load-size device-type)
    (* load-size
       (cdr (assq device-type
                  '((CUBIEBOARD . 4)
                    (SAMA5D3 .    2)))))))


(define spec (create-specification))

(define ast
  (with-specification
    spec
    (ast-rule 'Root->scheduler-Config)
    (ast-rule 'AbstractWorker->id-state-timestamp)
    (ast-rule 'CompositeWorker:AbstractWorker->AbstractWorker*<Workers)
    (ast-rule 'Config:CompositeWorker->)
    (ast-rule 'Switch:CompositeWorker->)
    (ast-rule 'Worker:AbstractWorker->devicetype-Request*<Queue)
    (ast-rule 'Request->id-size-deadline-dispatchtime)

    (compile-ast-specifications 'Root)

    (ag-rule
      lookup-worker
      (Worker
        (lambda (n id)
          (if (= id (ast-child 'id n))
            n
            #f)))
      (CompositeWorker
        (lambda (n id)
          (if (= id (ast-child 'id n))
            n
            (ast-find-child*
              (lambda (i worker)
                (att-value 'lookup-worker worker id))
              (ast-child 'Workers n))))))

    (ag-rule
      get-filtered-workers
      (Worker
        (lambda (n check)
          (if (check n) (list n) (list))))
      (CompositeWorker
        (lambda (n check)
          (fold-left
            (lambda (a b)
              (append a (att-value 'get-filtered-workers b check)))
            (list)
            (ast-children (ast-child 'Workers n))))))

    (ag-rule
      schedule
      (Root
        (lambda (n time work-id load-size deadline)
          (let
            ((scheduler (ast-child 'scheduler n)))
            (display ">Root schedule\n")
            (att-value scheduler (ast-child 'Config n) time work-id load-size deadline)))))


    (ag-rule
      schedule-robin
      (Config
        (lambda (n time work-id load-size deadline)
          (display ">Config schedule-robin\n")
          (let
            ((running-workers
               (att-value
                 'get-filtered-workers
                 n
                 (lambda (worker) (eq? (ast-child 'state worker) 'RUNNING)))))
            (if (null? running-workers)
              (cons #f #f)
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
                (cons worker index)))))))

    (ag-rule
      processing-timespan
      (Request
        (lambda (n)
          (let*
            ((queue (ast-parent n))
             (worker (ast-parent queue))
             (device-type (ast-child 'devicetype worker)))
            (predict-processing-timespan (ast-child 'size n) device-type)))))

    (ag-rule
      predicted-termination-time
      (Request
        (lambda (n)
          (let*
            ((queue (ast-parent n))
             (worker (ast-parent queue))
             (index (ast-child-index n))
             (device-type (ast-child 'devicetype worker)))
            (+ (att-value 'processing-timespan n)
               (if (= index 1)
                 (ast-child 'dispatchtime n) ; must not be #f
                 (att-value 'predicted-termination-time
                            (ast-child (- index 1) queue))))))))

    (ag-rule
      maximum-dispatch-deferment
      (Request
        (lambda (n)
          (let*
            ((index (ast-child-index n))
             (queue-length (ast-num-children (ast-parent n)))
             (local-deferment
               (-
                 (ast-child 'dispatchtime n)
                 (att-value 'predicted-termination-time n))))
            (if (= index queue-length)
              local-deferment
              (min local-deferment
                   (att-value 'maximum-dispatch-deferment
                              (ast-child (+ index 1) queue))))))))

    (ag-rule
      find-insertion-position
      (Worker
        (lambda (n deadline)
          (let*
            ((queue (ast-child 'Queue n))
             (queue-length (ast-num-children queue)))
            (if (= queue-length 0)
              1
              (let next ((index 2))
                (if (> index (queue-length))
                  index
                  (let ((next-deadline (ast-child 'deadline (ast-child index n))))
                    (if (> next-deadline deadline)
                      index
                      (next (+ index 1)))))))))))

    (ag-rule
      workload-heuristic
      (Worker
        (lambda (n)
          (let*
            ((queue (ast-child 'Queue n))
             (queue-length (ast-num-children queue)))
            (let next ((i 1) (sum 0))
              (if (> i queue-length)
                sum
                (next (+ i 1)
                      (+ sum (att-value 'processing-timespan (ast-child i queue)))))))))
      (Switch
        (lambda (n)
          (let*
            ((workers (ast-child 'Workers n))
             (num-workers (ast-num-children workers)))
            (let next ((i 1) (sum 0))
              (if (> i num-workers)
                sum
                (next (+ i 1)
                      (+ sum (att-value 'workload-heuristic (ast-child i workers))))))))))

    (ag-rule
      schedule-batman
      (Worker
        (lambda (n time work-id load-size deadline)
          (display ">Worker schedule-batman\n")
          ; hint: worker must be in state RUNNING
          (if (not (eq? (ast-child 'state n) 'RUNNING))
            (cons #f #f)
            (let*
              ((queue (ast-child 'Queue n))
               (queue-length (ast-num-children queue))
               (index (att-value 'find-insertion-position n deadline))
               (device-type (ast-child 'devicetype n))
               (processing-timespan (predict-processing-timespan load-size device-type))
               (dispatch-time
                 (if (= index 1)
                   time
                   (att-value 'predict-termination-time (ast-child (- index 1) queue))))
               (error
                 (or
                   (>
                     (+ dispatch-time processing-timespan)
                     deadline)
                   (and
                     (<= index queue-length)
                     (let
                       ((deferment (att-value 'maximum-dispatch-deferment (att-child index queue))))
                       (> processing-timespan deferment))))))
              (if error
                (cons #f #f)
                (cons n index))))))

      (CompositeWorker
        (lambda (n time work-id load-size deadline)
          (display ">CompositeWorker schedule-batman\n")
          ; hint: switch must be in state RUNNING
          (if (not (eq? (ast-child 'state n) 'RUNNING))
            (cons #f #f)
            (let*
              ((workers (ast-children (ast-child 'Workers n)))
               (sorted-workers
                 (list-sort
                   (lambda (a b)
                     (>
                       (att-value 'workload-heuristic a)
                       (att-value 'workload-heuristic b)))
                   workers)))
              (let next ((rest sorted-workers))
                (if (null? rest)
                  (cons #f #f)
                  (let*
                    ((pair (att-value 'schedule-batman (car rest) time work-id load-size deadline))
                     (w (car pair))
                     (i (cdr pair)))
                    (if w
                      pair
                      (next (cdr rest)))))))))))


    (compile-ag-specifications)

    (create-ast
      'Root
      (list
        ;'schedule-robin
        'schedule-batman
        (create-ast
          'Config
          (list 0 'RUNNING 0 (create-ast-list (list))))))))


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




(define adapt
  (lambda (num-backup-workers)
    (let*
      ((key
         (lambda (worker)
           (cdr (assq (ast-child 'devicetype worker)
                      '((CUBIEBOARD . 1)
                        (SAMA5D3 .    0))))))
       (idle-workers
         (att-value
           'get-filtered-workers
           (lambda (worker)
             (and
               (memq (ast-child 'state worker) '(RUNNING BOOTING))
               (= 0 (ast-num-children (ast-child 'Queue worker)))))
           config))
       (num-idle-workers (length idle-workers)))
      (cond
        ((> num-idle-workers num-backup-workers) ; halt
         (let
           ((num-excess-workers (- num-idle-workers num-backup-workers))
            (sorted-idle-workers
              (list-sort
                (lambda (a b) (< (key a) (key b)))
                idle-workers)))
           (let next ((i num-excess-workers) (rest sorted-idle-workers))
             (when (and (> i 0) (not (null? rest)))
               (let ((worker (car rest)))
                 (rewrite-terminal 'state worker 'HALTING)
                 (add-event
                   'event-halt-command
                   (ast-child 'id worker))
                 (next (- i 1) (cdr rest))))))
         ((< num-idle-workers num-backup-workers) ; boot
          (let*
            ((off-workers
               (att-value
                 'get-filtered-workers
                 (lambda (worker) (= (ast-child 'state worker) 'OFF))
                 config))
             (num-off-workers (length off-workers))
             (sorted-off-workers
               (list-sort
                 (lambda (a b) (> (key a) (key b)))
                 off-workers)))
             (num-wanting-workers (- num-backup-workers num-idle-workers)))
            (let next ((i num-wanting-workers) (rest sorted-off-workers))
              (when (and (> i 0) (not (null? rest)))
                (let ((worker (car rest)))
                  (rewrite-terminal 'state worker 'BOOTING)
                  (add-event
                    'event-worker-on
                    (ast-child 'id worker))
                  (next (- i 1) (cdr rest)))))))))))



(define dispatch-next-request
  (lambda (time worker)
    (let ((queue (ast-child 'Queue worker)))
      (if (not (= 0 (ast-num-children queue)))
        (let ((request (ast-child 1 queue)))
          (rewrite-terminal 'dispatchtime request time)
          (add-event
            'event-work-command
            (ast-child 'id worker)
            (ast-child 'id request)
            (ast-child 'size request)))
        #f))))

;;; events ;;;

(define event-worker-online
  (lambda (id time)
    (let ((worker (att-value 'lookup-worker config id)))
      (rewrite-terminal 'state worker 'RUNNING)
      (rewrite-terminal 'timestamp worker time)
      (dispatch-next-request time worker))))


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
  (lambda (time work-id load-size deadline)
    (let*
      ((pair (att-value 'schedule ast time work-id load-size deadline))
       (worker (car pair))
       (index (cdr pair)))
      (if worker
        (let*
          ((queue (ast-child 'Queue worker))
           (worker-idle? (= 0 (ast-num-children queue))))
          (rewrite-insert
            queue
            index
            (create-ast spec 'Request (list work-id load-size deadline #f)))
          (when worker-idle? (dispatch-next-request time worker)))
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
          (dispatch-next-request time worker))
        (display "no request with specified id found\n")))))


