(define spec (create-specification))

(define ast
 (with-specification spec
  (ast-rule 'Config->Box*<Boxes)
  (ast-rule 'Box->port-state-timestamp-Work*<Queue)
  (ast-rule 'Work->id-length-due)

  (compile-ast-specifications 'Config)

  (ag-rule
   lookup-box
   (Config
   (lambda (n port)
    (ast-find-child
     (lambda (i box) (= (ast-child 'port box) port))
     (ast-child 'Boxes n)))))

  (compile-ag-specifications)

  (create-ast 'Config
   (list
    (create-ast-list
     (let ((time (timestamp)))
      (map
       (lambda (port)
        (create-ast 'Box (list port 'OFF time (create-ast-list (list)))))
       (list 1 2 3 4 5 6))))))))


;(displayln (ast-child 'port (att-value 'lookup-box ast 3)))


(define get-box
 (lambda (port)
  (ast-find-child
   (lambda (i box) (= (ast-child 'port box) port))
   (ast-child 'Boxes ast))))

(define set-state
 (lambda (port state)
  (rewrite-terminal 'state (get-box port) state)))


(define id-counter 0)
(define next-id
 (lambda ()
  (set! id-counter (+ id-counter 1))
  id-counter))


(define push-work
 (lambda (port length due)
  (let ((box (get-box port)))
   (rewrite-add
    (ast-child 'Queue box)
    (create-ast spec 'Work (list (next-id) length due))))))

(define pop-work
 (lambda (port)
  (let ((box (get-box port)))
   (rewrite-delete
    (ast-child 1 (ast-child 'Queue box))))))

(set-state 1 'ON)
(push-work 1 32 60)
(push-work 1 32 60)
(push-work 1 32 60)
(pop-work 1)
(pop-work 1)


;(print-ast
; ast
; (list)
; (current-output-port))



(define display-ast
 (lambda ()
  (display "port | state | since | queue-length\n")
  (let ((time (timestamp)))
   (ast-for-each-child
    (lambda (i box)
     (display (ast-child 'port box))
     (display " ")
     (display (ast-child 'state box))
     (display " ")
     (display (/ (floor (* (- time (ast-child 'timestamp box)) 100)) 100))
     (display " ")
     (display (ast-num-children (ast-child 'Queue box)))
     (newline))
    (ast-child 'Boxes ast)))))

;(display-ast)

;(display "done.\n")


