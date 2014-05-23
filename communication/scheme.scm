(define hq-spec (create-specification))

(with-specification hq-spec

 (ast-rule 'Configurator->Configuration*-Profile*)
 (ast-rule 'Configuration->mode-memory)
 (ast-rule 'Profile->modes)

 (compile-ast-specifications 'Configurator)

 (compile-ag-specifications))


(display "done.\n")
