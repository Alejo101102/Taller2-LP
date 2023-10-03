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

;Función auxiliar para determinar el tamaño de una lista
(define (longitud-lista lst)
  (if (null? lst)
      0
      (+ 1 (longitud-lista (cdr lst)))))

;Implementacion por listas (falta documentación)

(define (crear-FNC numero-variables expresion)
  (list 'FNC numero-variables expresion))
 
(define (crear-expresion-final clausula)
  (list 'expresion clausula))

(define (crear-expresion clausula conjuncion expresion)
(list 'expresion clausula conjuncion expresion)  )

(define (crear-clausula-final literal)
  (list 'clausula literal))

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
  (and (list? expresion) (eq? 'expresion (car expresion)) (= (longitud-lista expresion) 2)))

(define (es-expresion? expresion)
  (and (list? expresion)
       (eq? 'expresion (car expresion))
       (eq? 'and (caddr expresion))))

(define (es-clausula-final? clausula)
  (and (list? clausula) (eq? 'clausula (car clausula))(= (longitud-lista clausula) 2)))

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

(define (obtener-numero-variables FNC)
  (if (es-FNC? FNC)
      (cadr FNC)
      (eopl:error 'obtener-numero-variables "Expecting expression, given ~s" FNC)))

(define (obtener-expresion FNC)
  (if (es-FNC? FNC)
      (caddr FNC)
      (eopl:error 'obtener-expresion "Expecting BNF expression, given ~s" FNC)))

(define (obtener-clausula expresion)
  (if (or (es-expresion-final? expresion) (es-expresion? expresion))
      (cadr expresion)
      (eopl:error 'obtener-clausula "Expecting expression, given ~s" expresion)))

(define (obtener-literal clausula)
  (if (or (es-clausula-final? clausula) (es-clausula? clausula))
      (cadr clausula)
      (eopl:error 'obtener-literal "Expecting clausula, given ~s" clausula)))

(define (obtener-variable literal)
  (if (es-literal? literal)
      (cadr literal)
      (eopl:error 'obtener-variable "Expecting literal, given ~s" literal)))

(define (obtener-numero variable)
  (if (es-variable? variable)
      (cadr variable)
      (eopl:error 'obtener-numero "Expecting variable, given ~s" variable)))

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


