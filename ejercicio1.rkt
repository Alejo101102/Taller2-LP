#lang eopl

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; TALLER #2 - FUNDAMENTOS DE COMPILACION E INTERPRETACION DE LP


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; INTEGRANTES
; Alejandro Guerrero Cano (2179652)- Alejo101102
; alejandro.cano@correounivalle.edu.co

; Juan David Loaiza Santiago (2177570)- JuanLoaiza007
; juan.loaiza.santiago@correounivalle.edu.co

; Juan Sebastian Muñoz Rojas (2177436)- sebastianmr18
; juan.munoz.rojas@correounivalle.edu.co


;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; DESARROLLO

;Gramatica BNF

;<FNC> ::= 'FNC <numero-variables> <expresion>
;<numero-variables> ::= <int>
;<expresion> ::= <clausula> | <clausula> <conjuncion> <expresion>
;<conjuncion> ::= 'and
;<clausula> ::= <literal> | <literal> <disyunción> <clausula>
;<disyunción> ::= 'or
;<literal> ::= <variable>
;<variable> ::= <int>

;; Implementacion de ambientes (codigo de clase)
;; empty-env
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;; apply-env
(define apply-env
  (lambda (env search-var)
    (cond ((eqv? (car env) 'empty-env)
           (eopl:error 'apply-env "No binding for ~s" search-var))
          ((eqv? (car env) 'extend-env)
           (let ((saved-var (cadr env))
                 (saved-val (caddr env))
                 (saved-env (cadddr env)))
             (if (eqv? search-var saved-var)
                 saved-val
                 (apply-env saved-env search-var))))
          (else (eopl:error 'apply-env "Expecting an environment, given ~s" env)))))

;Función auxiliar para determinar el tamaño de una lista
(define (longitud-lista lst)
  (if (null? lst)
      0
      (+ 1 (longitud-lista (cdr lst)))))

(define cons-end
  (lambda (lista elemento)
    (cond
      ((null? lista) elemento)
      (else (cons (car lista)
                  (cons-end (cdr lista) elemento))))))

(define negar
  (lambda (valor-booleano)
    (if (eqv? valor-booleano #t) #f #t)
    ))

;Implementacion por listas (falta documentación)
(define (crear-FNC numero-variables expresion)
  (list 'FNC numero-variables expresion))

(define (crear-expresion-final clausula)
  (list 'expresion-final clausula))

(define (crear-expresion clausula conjuncion expresion)
  (list 'expresion clausula conjuncion expresion)  )

(define (crear-clausula-final literal)
  (list 'clausula-final literal))

(define (crear-clausula literal disyuncion clausula)
  (list 'clausula literal disyuncion clausula))

(define (crear-literal variable)
  (list 'literal variable))

(define (crear-variable num)
  (list 'variable num))

(define (crear-conjuncion)
  'and)

(define (crear-disyuncion)
  'or)

(define (es-FNC? FNC)
  (and (list? FNC)
       (= (longitud-lista FNC) 3)
       (eq? (car FNC) 'FNC)
       (number? (cadr FNC))))

(define (es-expresion-final? expresion)
  (and (list? expresion) (eq? 'expresion-final (car expresion))))

(define (es-expresion? expresion)
  (and (list? expresion)
       (eq? 'expresion (car expresion))
       (eq? 'and (caddr expresion))))

(define (es-clausula-final? clausula)
  (and (list? clausula) (eq? 'clausula-final (car clausula))))

(define (es-clausula? clausula)
  (and (list? clausula)
       (eq? 'clausula (car clausula))
       (eq? 'or (caddr clausula))))

(define (es-literal? literal)
  (and (list? literal) (eq? 'literal (car literal))))

(define (es-variable? variable)
  (and (list? variable) (eq? 'variable (car variable))))

(define (es-conjuncion? expr)
  (eq? expr 'and))

(define (es-disyuncion? expr)
  (eq? expr 'or))

(define (es-numero? expr)
  (number? expr))

(define (obtener-numero-variables FNC)
  (if (es-FNC? FNC)
      (cadr FNC)
      (eopl:error 'obtener-numero-variables "Expecting expression, given ~s" FNC)))

(define (obtener-expresion FNC)
  (if (es-FNC? FNC)
      (caddr FNC)
      (eopl:error 'obtener-expresion "Expecting BNF expression, given ~s" FNC)))

(define (obtener-expresion-interna expresion)
  (cadddr expresion))

(define (obtener-clausula expresion)
  (if (or (es-expresion-final? expresion) (es-expresion? expresion))
      (cadr expresion)
      (eopl:error 'obtener-clausula "Expecting expression, given ~s" expresion)))

(define (obtener-literal clausula)
  (if (or (es-clausula-final? clausula) (es-clausula? clausula))
      (cadr clausula)
      (eopl:error 'obtener-literal "Expecting expression, given ~s" clausula)))

(define (obtener-variable literal)
  (if (es-literal? literal)
      (cadr literal)
      (eopl:error 'obtener-variable "Expecting expression, given ~s" literal)))

(define (obtener-numero variable)
  (if (es-variable? variable)
      (cadr variable)
      (eopl:error 'obtener-numero "Expecting expression, given ~s" variable)))

; Ejemplos de instancias SAT
(define expresion-1 ; 0
  (crear-expresion-final
   (crear-clausula-final
    (crear-literal
     (crear-variable 0)))))

(define expresion-2 ; 1
  (crear-expresion-final
   (crear-clausula-final
    (crear-literal
     (crear-variable 1)))))


(define expresion-3 ; 2
  (crear-expresion-final
   (crear-clausula-final
    (crear-literal
     (crear-variable 2)))))

(define expresion-FNC-e ; 1 and 2 and 3
  (crear-FNC 3
             (crear-expresion
              (crear-clausula-final
               (crear-literal
                (crear-variable 1)))

              (crear-conjuncion)

              (crear-expresion
               (crear-clausula-final
                (crear-literal
                 (crear-variable 2)))

               (crear-conjuncion)

               (crear-expresion-final
                (crear-clausula-final
                 (crear-literal
                  (crear-variable 3))))))))

(define expresion-FNC-2 ; 1 and 2 and (3 or 4)
  (crear-FNC 4 ; Supongamos que 4 es el número que deseas
             (crear-expresion
              (crear-clausula-final
               (crear-literal
                (crear-variable 1)))
              (crear-conjuncion)
              (crear-expresion
               (crear-clausula-final
                (crear-literal
                 (crear-variable 2)))
               (crear-conjuncion)
               (crear-expresion-final
                (crear-clausula
                 (crear-literal
                  (crear-variable 3))
                 (crear-disyuncion)
                 (crear-clausula-final
                  (crear-literal
                   (crear-variable 4)))))))))


; Implementación con datatype
; Falta documentación y resolver conjunción y disyunción
(define-datatype expresion-FNC expresion-FNC?
  (FNC (numero-variables number?)(expr expresion?))
  )

(define-datatype expresion expresion?
  (expresion-final (clausula clausula?))
  (expresion-no-final (clausula clausula?)
                      (expresion expresion?))
  )

(define-datatype clausula clausula?
  (clausula-final (literal literal?))
  (clausula-no-final (literal literal?)
                     (clausula clausula?))
  )

(define-datatype literal literal?
  (lit (variable variable?))
  )

(define-datatype variable variable?
  (numero (num number?))
  )

(define conjuncion?
  (list 'and)
  )

(define disyuncion?
  (list 'or)
  )

(define expr-sat-1 ; 6
  (FNC 1
       (expresion-final
        (clausula-final
         (lit (numero 6))))))

(define expresion-sat-doble ; (1 or -1)and 2
  (FNC 2
       (expresion-no-final
        (clausula-no-final
         (lit (numero 1))
         ;(disyuncion (or ('or)))
         (clausula-final
          (lit (numero -1))))
        ;(conjuncion (and))
        (expresion-final
         (clausula-final
          (lit (numero 2)))))))

;; 3) Evaluador ;; En Desarrollo
(define (EVALUARSAT FNC)
  (let ((solucion (explorar-solucion FNC (obtener-numero-variables FNC) '())))
    (if (null? solucion)
        (list 'insatisfactible '())
        (list 'satisfactible solucion))))

(define (explorar-solucion FNC num-variables asignacion-actual)
  (if (= (longitud-lista asignacion-actual) num-variables)
      (if (evaluar-FNC FNC asignacion-actual)
          asignacion-actual
          '())
      (let (
            (asignacion-true (cons-end asignacion-actual (list #t)))
            (asignacion-false (cons-end asignacion-actual (list #f))))
        (letrec (
                 (solucion-true (explorar-solucion FNC num-variables asignacion-true))
                 (solucion-false (explorar-solucion FNC num-variables asignacion-false)))
          (if (not (null? solucion-true))
              solucion-true
              solucion-false)
          )
        )
      )
  )

(define (evaluar-FNC FNC asignacion)
  (let ((expresion (obtener-expresion FNC)))
    (evaluar-expresion expresion asignacion)
    ))

(define (evaluar-expresion expresion asignacion)
  #| Desarrollador
  (display "\n\n Soy evaluar-expresion \n")
  (display "Expresion es: --> ") (display expresion)
  (display "\n")
  (display "Asignacion es: --> ") (display asignacion)
  (display "\n")
   |#

  (cond
    ((es-expresion-final? expresion)
     (evaluar-clausula (obtener-clausula expresion) asignacion)
     )
    ((es-expresion? expresion)
     (cond
       ((es-conjuncion? (caddr expresion))
        (
         and (evaluar-clausula (obtener-clausula expresion) asignacion)
             (evaluar-expresion (obtener-expresion-interna expresion) asignacion)
             ))
       ((es-disyuncion? (caddr expresion))
        (
         or (evaluar-clausula (obtener-clausula expresion) asignacion)
            (evaluar-expresion (obtener-expresion-interna expresion) asignacion)
            ))
       ))
    (else
     (eopl:error 'evaluar-expresion "Entrada no válida: se esperaba una expresión, se recibió ~s" expresion))))

(define (evaluar-clausula clausula asignacion)
  #| Desarrollador
  (display "\n\n Soy evaluar-clausula \n")
  (display "Clausula es: --> ") (display clausula)
  (display "\n")
  (display "Asignacion es: --> ") (display asignacion)
  (display "\n")
  (display "Literal es: --> ") (display (obtener-literal clausula))
  (display "\n")
  |#

  (cond
    ((es-clausula-final? clausula)
     (evaluar-literal (obtener-literal clausula) asignacion))
    ((es-clausula? clausula)
     (cond
       ((es-conjuncion? (caddr clausula))
        (and (evaluar-literal (obtener-literal clausula) asignacion)
             (evaluar-clausula (obtener-clausula clausula) asignacion)))
       ((es-disyuncion? (caddr clausula))
        (or (evaluar-literal (obtener-literal clausula) asignacion)
            (evaluar-clausula (obtener-clausula clausula) asignacion))))
     )
    (else
     (eopl:error 'evaluar-clausula "Entrada no válida: se esperaba una cláusula, se recibió ~s" clausula)))
  )

(define (evaluar-literal literal asignacion)
  (let ((variable (obtener-variable literal)))
    (let ((valor (obtener-numero variable)))
      (if (es-variable? variable)
          (if (< valor 0)
              (negar (car asignacion))
              (car asignacion))

          (eopl:error 'evaluar-literal "Entrada no válida: se esperaba una variable, se recibió ~s" variable)))))

;; Conejillos de indias
(define basicFNC1 ; x
  (crear-FNC 1
             (crear-expresion-final
              (crear-clausula-final
               (crear-literal
                (crear-variable 3))))))

(define basicFNC1-1 ; -x
  (crear-FNC 1
             (crear-expresion-final
              (crear-clausula-final
               (crear-literal
                (crear-variable -3))))))

(define basicFNC2-1 ; x and y
  (crear-FNC 2
             (crear-expresion
              (crear-clausula-final
               (crear-literal
                (crear-variable 2)))

              (crear-conjuncion)

              (crear-expresion-final
               (crear-clausula-final
                (crear-literal
                 (crear-variable 1)))))))

(define basicFNC2-2 ; x and -y
  (crear-FNC 2
             (crear-expresion
              (crear-clausula-final
               (crear-literal
                (crear-variable 2)))

              (crear-conjuncion)

              (crear-expresion-final
               (crear-clausula-final
                (crear-literal
                 (crear-variable -1)))))))


(define basicFNC3 ; x and y and z
  (crear-FNC 3
             (crear-expresion
              (crear-clausula-final
               (crear-literal
                (crear-variable -1)))

              (crear-conjuncion)

              (crear-expresion
               (crear-clausula-final
                (crear-literal
                 (crear-variable -2)))

               (crear-conjuncion)

               (crear-expresion-final
                (crear-clausula-final
                 (crear-literal
                  (crear-variable -3))))))))

(define basicFNC1-0 ;; -x and x
  (crear-FNC 1
             (crear-expresion
              (crear-clausula-final
               (crear-literal
                (crear-variable -1)))

              (crear-conjuncion)

              (crear-expresion-final
               (crear-clausula-final
                (crear-literal
                 (crear-variable 1)))
               )
              )))

(display "\n")
;; x
(display "basicFNC1: ") (display (EVALUARSAT basicFNC1)) (display "\n\n") ;; (satisfactible (#t))
;; -x
(display "basicFNC1-1: ") (display (EVALUARSAT basicFNC1-1)) (display "\n\n") ;; (satisfactible (#f))
;; -x and x
(display "basicFNC1-0: ") (display (EVALUARSAT basicFNC1-0)) (display "\n\n") ;; (insatisfactible ())
;; x and y
(display "basicFNC2-1: ") (display (EVALUARSAT basicFNC2-1)) (display "\n\n") ;; (satisfactible (#t #t))
;; x and -y
(display "basicFNC2-2: ") (display (EVALUARSAT basicFNC2-2)) (display "\n\n") ;; (satisfactible (#t #f))
;; x and y and z
(display "basicFNC3: ") (display (EVALUARSAT basicFNC3)) (display "\n\n") ;; (satisfactible (#t #t #t))