(import (rnrs) (racr))

(define energy-consumption
 (lambda (flops)
  (let loop ((l (list '(0 . 4) '(2 . 5) '(10 . 10))))
   (if (<= (caadr l) flops)
    (loop (cdr l))
    (+ (cdar l)
     (* (/ (- flops (caar l))
         (- (caadr l) (caar l)))
      (- (cdadr l) (cdar l))))))))

(define haec-specification
 (create-specification))


(define ast
 (with-specification haec-specification
  (ast-rule 'Configurator->ProgramProfile*-MachineProfile*-CurrentConfig*)
  (ast-rule 'ProgramProfile->)
  (ast-rule 'MachineProfile->)
  (ast-rule 'CurrentConfig->)
  (compile-ast-specifications 'Configurator)
  (compile-ag-specifications)
  (create-ast
   'Configurator
   (list
    (create-ast-list (list))
    (create-ast-list (list))
    (create-ast-list (list))))))


