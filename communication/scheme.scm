; TODO:
; C:
; - keep state.txt
; Web:
; Scheme:
; - return error message from scheduler


(define-record-type
  device-characteristic
  (fields
    bootable              ; #t or #f
    boot-timespan         ; in s
    halt-timespan         ; in s
    boot-current-integral ; in mA*s
    halt-current-integral ; in mA*s
    speed                 ; in MB/s
    idle-current          ; in mA
    busy-current))        ; in mA

(define device-table (make-eq-hashtable))
(hashtable-set!
  device-table
  'CUBIEBOARD
  (make-device-characteristic
    #t
    60.0
    13.0
    (* 60.0 300.0)
    (* 13.0 300.0)
    3.5
    250.0
    400.0))

(hashtable-set!
  device-table
  'SAMA5D3
  (make-device-characteristic
    #f
    0.0
    0.0
    0.0
    0.0
    1.0
    150.0
    200.0))



;(define say (lambda (what) (display what)))
;(define sayln (lambda (what) (displayln what)))
(define say (lambda (what) #f))
(define sayln (lambda (what) #f))



(define predict-processing-timespan
  (lambda (load-size device-type)
    (say "[FUNCTION] predict-processing-timespan\n")
    (/ load-size
       (device-characteristic-speed (hashtable-ref device-table device-type #f)))))


(define spec (create-specification))

(define ast
  (with-specification
    spec
    (ast-rule 'Root->scheduler-backupworkers-Config)
    (ast-rule 'AbstractWorker->id-state-timestamp)
    (ast-rule 'CompositeWorker:AbstractWorker->AbstractWorker*<Workers)
    (ast-rule 'Config:CompositeWorker->)
    (ast-rule 'Switch:CompositeWorker->)
    (ast-rule 'Worker:AbstractWorker->devicetype-Request*<Queue)
    (ast-rule 'Request->id-size-deadline-dispatchtime)

    (compile-ast-specifications 'Root)

    (ag-rule
      get-backup-workers
      (Root
        (lambda (n)
          (let
            ((backup-strategy (ast-child 'backupworkers n)))
             (cond
               ((eq? backup-strategy 'one-two)   (determine-num-backup-workers 1 2))
               ((eq? backup-strategy 'one-three) (determine-num-backup-workers 1 3))
               ((eq? backup-strategy 'magic)     (determine-num-backup-workers 2 3))
               ((eq? backup-strategy 'magic8)     (determine-num-backup-workers 20 30))
               (else backup-strategy))))))


    (ag-rule
      lookup-worker
      (Worker
        (lambda (n id)
          (say "[ATTRIBUTE on Worker] lookup-worker\n")
          (if (= id (ast-child 'id n))
            n
            #f)))
      (CompositeWorker
        (lambda (n id)
          (say "[ATTRIBUTE on CompositeWorker] lookup-worker\n")
          (if (= id (ast-child 'id n))
            n
            (ast-find-child*
              (lambda (i worker)
                (att-value 'lookup-worker worker id))
              (ast-child 'Workers n))))))

    (ag-rule
      ; returns a list of all workers (excluding switches) that satisfy a given condition
      get-filtered-workers
      (Worker
        #f
        (lambda (n check)
          (say "[ATTRIBUTE on Worker ")
          (say (ast-child 'id n))
          (say "] get-filtered-workers: ")
          (sayln (check n))
          (if (check n) (list n) (list))))
      (CompositeWorker
        #f
        (lambda (n check)
          (say "[ATTRIBUTE on CompositeWorker ")
          (say (ast-child 'id n))
          (sayln "] get-filtered-workers")
          (fold-left
            (lambda (a b)
              (append a (att-value 'get-filtered-workers b check)))
            (list)
            (ast-children (ast-child 'Workers n))))))


    (ag-rule
      depth
      (Config
        (lambda (n) 0))
      (AbstractWorker
        (lambda (n) (+ 1 (att-value 'depth (ast-parent n))))))


    (ag-rule
      in-use?
      (Config (lambda (n) #t))
      (Switch
        (lambda (n)
          (exists
            (lambda (child)
              (memq (ast-child 'state child) '(RUNNING BOOTING)))
            (ast-children (ast-child 'Workers n))))))


    (ag-rule
      schedule
      (Root
        (lambda (n time work-id load-size deadline)
          (say "[ATTRIBUTE on Root] schedule\n")
          (let
            ((scheduler (ast-child 'scheduler n)))
            (att-value scheduler (ast-child 'Config n) time work-id load-size deadline)))))


    (ag-rule
      schedule-robin
      (Config
        (lambda (n time work-id load-size deadline)
          (say "[ATTRIBUTE on Config] schedule-robin\n")
          (let
            ((running-workers
               (att-value
                 'get-filtered-workers
                 n
                 (lambda (worker) (eq? (ast-child 'state worker) 'RUNNING)))))
            (if (null? running-workers)
              (cons #f "no worker running.")
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
          (say "[ATTRIBUTE on Request] processing-timespan\n")
          (let*
            ((queue (ast-parent n))
             (worker (ast-parent queue))
             (device-type (ast-child 'devicetype worker)))
            (predict-processing-timespan (ast-child 'size n) device-type)))))

    (ag-rule
      predicted-termination-time
      (Request
        (lambda (n)
          (say "[ATTRIBUTE on Request] predicted-termination-time\n")
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
          (say "[ATTRIBUTE on Request] maximum-dispatch-deferment\n")
          (let*
            ((index (ast-child-index n))
             (queue (ast-parent n))
             (queue-length (ast-num-children queue))
             (local-deferment
               (-
                 (ast-child 'deadline n)
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
          (say "[ATTRIBUTE on Worker] find-insertion-position\n")
          (let*
            ((queue (ast-child 'Queue n))
             (queue-length (ast-num-children queue)))
            (if (= queue-length 0)
              (begin
                (say "  CASE 1: queue length=0\n")
                1)
              (begin
                (say "  CASE 2: queue length>0\n")
                (let next ((index 2))
                  (if (> index queue-length)
                    (begin
                      (say "    CASE 2 A: index > queue-length\n")
                      index)
                    (begin
                      (say "    CASE 2 B: index <= queue-length\n")
                      (let ((next-deadline (ast-child 'deadline (ast-child index (ast-child 'Queue n)))))
                        (if (> next-deadline deadline)
                          index
                          (next (+ index 1)))))))))))))

    (ag-rule
      workload-heuristic
      (Worker
        (lambda (n)
          (say "[ATTRIBUTE on Worker] workload-heuristic\n")
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
          (say "[ATTRIBUTE on Switch] workload-heuristic\n")
          (let*
            ((workers (ast-child 'Workers n))
             (num-workers (ast-num-children workers)))
            (let next ((i 1) (sum -1000000))
              (if (> i num-workers)
                sum
                (next (+ i 1)
                      (+ sum (att-value 'workload-heuristic (ast-child i workers))))))))))


    (ag-rule
      schedule-batman
      (Worker
        (lambda (n time work-id load-size deadline)
          (say "[ATTRIBUTE on Worker] schedule-batman\n")
          ; hint: worker must be in state RUNNING
          (if (not (eq? (ast-child 'state n) 'RUNNING))
            (cons #f (format "worker ~a is not running." (ast-child 'id n)))
            (let*
              ((queue (ast-child 'Queue n))
               (queue-length (ast-num-children queue))
               (index (att-value 'find-insertion-position n deadline))
               (device-type (ast-child 'devicetype n))
               (processing-timespan (predict-processing-timespan load-size device-type))
               (dispatch-time
                 (if (= index 1)
                   time
                   (att-value 'predicted-termination-time (ast-child (- index 1) queue))))
               (error
                 (cond
                   ((> (+ dispatch-time processing-timespan) deadline)
                    "deadline cannot be met.")
                   ((and
                      (<= index queue-length)
                      (let
                        ((deferment (att-value 'maximum-dispatch-deferment (ast-child index queue))))
                        (> processing-timespan deferment)))
                    "requests cannot be defered.")
                   (#t #f))))
              (if error
                (cons #f error)
                (cons n index))))))

      (CompositeWorker
        (lambda (n time work-id load-size deadline)
          (say "[ATTRIBUTE on CompositeWorker] schedule-batman\n")
          ; hint: switch must be in state RUNNING
          (if (not (eq? (ast-child 'state n) 'RUNNING))
            (cons #f (format "composite worker ~a is not running." (ast-child 'id n)))
            (let*
              ((workers (ast-children (ast-child 'Workers n)))
               (sorted-workers
                 (list-sort
                   (lambda (a b)
                     (>
                       (att-value 'workload-heuristic a)
                       (att-value 'workload-heuristic b)))
                   workers)))
              ;(say (length sorted-workers))
              (let next ((rest sorted-workers))
                (if (null? rest)
                  (cons #f (format "request at ~A could not be scheduled." time))
                  (let*
                    ((pair (att-value 'schedule-batman (car rest) time work-id load-size deadline))
                     (w (car pair))
                     (i (cdr pair)))
                    ;(say i)
                    (if w
                      pair
                      (next (cdr rest)))))))))))


    (compile-ag-specifications)

    (create-ast
      'Root
      (list
        'schedule-robin
        'one-three
        ;'schedule-batman
        (create-ast
          'Config
          (list 0 'RUNNING 0 (create-ast-list (list))))))))


(define say-ast
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


(define set-backup-workers
  (lambda (number)
    (rewrite-terminal 'backupworkers ast number)))



(define determine-num-backup-workers
  (lambda (min-num-backup-workers min-num-running-workers)
    (say "[FUNCTION] determine-num-backup-workers\n")
    (let*
      ((working-workers
         (att-value
           'get-filtered-workers
           config
           (lambda (w) (< 0 (ast-num-children (ast-child 'Queue w))))))
       (num-working-workers (length working-workers)))
      (max min-num-backup-workers
           (- min-num-running-workers num-working-workers)))))


; only the bootable workers can be considered.
(define adapt
  (lambda (time)
    (say "[FUNCTION] adapt\n")
    (let*
      ((num-backup-workers (att-value 'get-backup-workers ast))
       (idle-workers
         (att-value
           'get-filtered-workers
           config
           (lambda (worker)
             (and
               (memq (ast-child 'state worker) '(RUNNING BOOTING))
               (= 0 (ast-num-children (ast-child 'Queue worker)))))))
       (num-idle-workers (length idle-workers))
       (haltable-workers
         (att-value
           'get-filtered-workers
           config
           (lambda (worker)
             (and
               (memq (ast-child 'state worker) '(RUNNING BOOTING))
               (= 0 (ast-num-children (ast-child 'Queue worker)))
               (>= (- time (ast-child 'timestamp worker)) 60)))))
       (num-haltable-workers (length haltable-workers)))
      (cond
        ((> num-haltable-workers num-backup-workers) ; halt
         (sayln ">>>>>> HALT!")
         (let*
           ((num-excess-workers (- num-haltable-workers num-backup-workers))
            (sorted-haltable-workers
              (list-sort
                (lambda (a b)
                  (or
                    (> (att-value 'depth a)
                       (att-value 'depth b))
                    (and
                      (= (att-value 'depth a)
                         (att-value 'depth b))
                      (or
                        (and
                          (eq? (ast-child 'state a) 'BOOTING)
                          (not (eq? (ast-child 'state b) 'BOOTING)))
                        (and
                          (eq? (ast-child 'state a) 'BOOTING)
                          (eq? (ast-child 'state b) 'BOOTING)
                          (> (ast-child 'id a) (ast-child 'id b)))))))
                haltable-workers))
            (sorted-bootable-haltable-workers
              (filter
                (lambda (x)
                  (let*
                    ((devicetype (ast-child 'devicetype x))
                     (bootable (device-characteristic-bootable (hashtable-ref device-table devicetype "error!"))))
                    (and
                      (>= (- time (ast-child 'timestamp x)) 30)
                      bootable)))
                  sorted-haltable-workers)))
           (let next ((i num-excess-workers) (rest sorted-bootable-haltable-workers))
             (when (and (> i 0) (not (null? rest)))
               (let ((worker (car rest)))
                 (rewrite-terminal 'state worker 'HALTING)
                 (add-event 'event-halt-command (ast-child 'id worker))
                 (next (- i 1) (cdr rest)))))))
        ((< num-idle-workers num-backup-workers) ; boot
         (sayln ">>>>>> BOOT!")
         (let*
           ((off-workers
              (att-value
                'get-filtered-workers
                config
                (lambda (worker) (eq? (ast-child 'state worker) 'OFF))))
            (num-off-workers (length off-workers))
            (sorted-off-workers
              (list-sort
                (lambda (a b)
                  (< (att-value 'depth a)
                     (att-value 'depth b)))
                off-workers))
            (sorted-bootable-off-workers
              (filter
                (lambda (x)
                  (let*
                    ((devicetype (ast-child 'devicetype x))
                     (bootable (device-characteristic-bootable (hashtable-ref device-table devicetype "error!"))))
                    bootable))
                  sorted-off-workers))
            (num-wanting-workers (- num-backup-workers num-idle-workers)))
           (let next ((i num-wanting-workers) (rest sorted-bootable-off-workers))
             (when (and (> i 0) (not (null? rest)))
               (let ((worker (car rest)))
                 (rewrite-terminal 'state worker 'BOOTING)
                 (rewrite-terminal 'timestamp worker time)
                 (add-event 'event-worker-on (ast-child 'id worker))

                 ; check whether switches should be turned on
                 (let next ((parent (ast-parent (ast-parent worker))))
                   (when (eq? (ast-child 'state parent) 'OFF)
                     (begin
                       (rewrite-terminal 'state parent 'RUNNING)
                       (add-event 'event-switch-on (ast-child 'id parent))
                       (next (ast-parent (ast-parent parent))))))

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
      (dispatch-next-request time worker)

      ; check whether switches should have been turned on :D
      (let next ((parent (ast-parent (ast-parent worker))))
        (when (eq? (ast-child 'state parent) 'OFF)
          (begin
            (rewrite-terminal 'state parent 'RUNNING)
            (add-event 'event-switch-on (ast-child 'id parent))
            (next (ast-parent (ast-parent parent)))))))))


(define event-worker-offline
  (lambda (id time)
    (let ((worker (att-value 'lookup-worker config id)))
      (when (not (eq? (ast-child 'state worker) 'HALTING))
        (begin
          (rewrite-terminal 'state worker 'ERROR)
          (rewrite-terminal 'timestamp worker time)))

      ; check whether switches can be turned off
      (let next ((parent (ast-parent (ast-parent worker))))
        (when (not (att-value 'in-use? parent))
          (begin
            (rewrite-terminal 'state parent 'OFF)
            (add-event 'event-switch-off (ast-child 'id parent))
            (next (ast-parent (ast-parent parent)))))))))


(define event-worker-off
  (lambda (id time)
    (let ((worker (att-value 'lookup-worker config id)))
      (rewrite-terminal 'state worker 'OFF)
      (rewrite-terminal 'timestamp worker time)

      ; check whether switches can be turned off
      (let next ((parent (ast-parent (ast-parent worker))))
        (when (not (att-value 'in-use? parent))
          (begin
            (rewrite-terminal 'state parent 'OFF)
            (add-event 'event-switch-off (ast-child 'id parent))
            (next (ast-parent (ast-parent parent)))))))))

(define event-adapt
  (lambda (time)
    (adapt time)))


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
        (printf "[ERROR] ~a~n" index)))))


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
;          (let*
;            ((processing-time (- time (ast-child 'dispatchtime request)))
;             (speed (/ (ast-child 'size request) processing-time))
;             (deadline (ast-child 'deadline request))
;             (remaining-time (- deadline time))
;             (met-deadline? (<= 0 remaining-time)))
;            (printf "### ~a ~a ~a ~a ~a~n" (ast-child 'size request) speed processing-time met-deadline? remaining-time))

          (rewrite-terminal 'timestamp worker time)
          (rewrite-delete request)
          (dispatch-next-request time worker))
        (say "[FATAL ERROR] no request with specified id found\n"))))) ; this should never happen


