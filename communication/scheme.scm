(define spec (create-specification))

(define ast
 (with-specification spec
  (ast-rule 'Config->Worker*<Workers-Switch*<Switches)
  (ast-rule 'Worker->id-state-timestamp-Work*<Queue)
  (ast-rule 'Work->id-size-time)
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
 (lambda (id time switch_id)
  (rewrite-add workers
   (create-ast spec 'Worker (list id 'OFF time (create-ast-list (list)))))))

(define add-switch-to-ast
 (lambda (id time)
  (rewrite-add switches
   (create-ast spec 'Switch (list id 'OFF time)))))



;(displayln (ast-child 'id (att-value 'lookup-worker ast 3)))


(define get-worker
 (lambda (id)
  (ast-find-child
   (lambda (i worker) (= (ast-child 'id worker) id))
   (ast-child 'Workers ast))))

(define set-state
 (lambda (id state)
  (rewrite-terminal 'state (get-worker id) state)))


(define id-counter 0)
(define next-id
 (lambda ()
  (set! id-counter (+ id-counter 1))
  id-counter))


(define push-work
 (lambda (id size time)
  (let ((worker (get-worker id)))
   (rewrite-add
    (ast-child 'Queue worker)
    (create-ast spec 'Work (list (next-id) size time))))))

(define pop-work
 (lambda (id)
  (let ((worker (get-worker id)))
   (rewrite-delete
    (ast-child 1 (ast-child 'Queue worker))))))

;(set-state 1001 'ON)
;(push-work 1001 32 60)
;(push-work 1001 32 60)
;(push-work 1001 32 60)
;(pop-work 1001)


;(print-ast
; ast
; (list)
; (current-output-id))



(define display-ast
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
   (rewrite-terminal 'state worker 'IDLE)
   (rewrite-terminal 'timestamp worker time)
   (display-ast))))

(define event-worker-off
 (lambda (id time)
  (let ((worker (get-worker id)))
   (rewrite-terminal 'state worker 'OFF)
   (rewrite-terminal 'timestamp worker time))))

;  (let*
;   ((worker (get-worker id))
;    (state (ast-child 'state worker)))
;   (rewrite-terminal 'state worker
;    (if (eq? state 'HALTING)
;     'OFF
;     'ERROR))
;   (rewrite-terminal 'timestamp worker time)
;   (display-ast))))


(define event-work-request
 (lambda (load-size time-due)
  (display "event-work-request\n")))


;(let*
; ((workers (ast-child 'Workers ast))
;  (count (ast-num-children workers)))
; (display count)
; (newline)
; (display (ast-child 1 workers))
; (newline))
