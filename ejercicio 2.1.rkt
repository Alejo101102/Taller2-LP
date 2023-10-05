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

;; Punto 1

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

(define (obtener-expresion-interna FNC)
  (if (es-FNC? FNC)
      (cadddr (caddr FNC))
      (eopl:error 'obtener-expresion "Expecting BNF expression, given ~s" FNC)))

(define (obtener-clausula expresion)
  (if (or (es-expresion-final? expresion) (es-expresion? expresion))
      (cadr expresion)
      (eopl:error 'obtener-clausula "Expecting expression, given ~s" expresion)))

(define (obtener-clausula-interna expresion)
  (if (or (es-expresion-final? expresion) (es-expresion? expresion))
      (cadddr (cadr expresion))
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


;; Punto 2

(define (PARSEBNF entrada)
  (cond
    ((number? entrada) (crear-literal (crear-variable entrada)))
    ((eq? (car entrada) 'or) (crear-expresion-final (crear-clausula-final (aplicar-operador 'or (cdr entrada)))))
    ((eq? (car entrada) 'and) (crear-expresion-final (crear-clausula-final (aplicar-operador 'and (cdr entrada)))))
    ((eq? (car entrada) 'not) (crear-expresion-final (crear-clausula-final (aplicar-not (cdr entrada)))))
    (else (eopl:error "Entrada no válida"))))

(define (aplicar-not elementos)
  (cond
    ((null? elementos) (eopl:error "Operador 'not' debe tener un argumento"))
    ((null? (cdr elementos)) (crear-literal (crear-variable (list 'not (PARSEBNF (car elementos))))))
    (else (eopl:error "Operador 'not' solo debe tener un argumento"))))


(define (aplicar-operador operador elementos)
  (cond
    ((null? elementos) (if (eq? operador 'or) 'F 'T))
    ((null? (cdr elementos)) (PARSEBNF (car elementos)))
    (else (crear-clausula (crear-literal (PARSEBNF (car elementos))) operador (aplicar-operador operador (cdr elementos))))))


; Ejemplo de uso:
(define entrada-ejemplo '(and 1 (or 2 (not 3)) 5))
(define do (PARSEBNF entrada-ejemplo))
(display do)


