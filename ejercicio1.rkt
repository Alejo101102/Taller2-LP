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

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;Implementación Gramatica BNF

;<FNC> ::= 'FNC <numero-variables> <expresion>
;<numero-variables> ::= <int>
;<expresion> ::= <clausula> | <clausula> <conjuncion> <expresion>
;<conjuncion> ::= 'and
;<clausula> ::= <literal> | <literal> <disyunción> <clausula>
;<disyunción> ::= 'or
;<literal> ::= <variable>
;<variable> ::= <int>

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; funciones auxiliares

;; longitud-lista
;; La función longitud-lista calcula la longitud (cantidad de elementos)
;; de una lista dada. Funciona de manera recursiva, incrementando un contador
;; en 1 por cada elemento de la lista hasta que la lista esté vacía,
;; momento en el que devuelve el contador acumulado, que es la longitud total de la lista.
;; Esta función se emplea para obtener la cantidad de elementos presentes en una lista,
;; lo que puede ser útil en diversos contextos como manipulación y análisis de listas.
(define (longitud-lista lst)
  (if (null? lst)
      0
      (+ 1 (longitud-lista (cdr lst)))))

; Caso de prueba 1: Lista vacía
(longitud-lista '()) ;; Resultado esperado: 0
; Caso de prueba 2: Lista con elementos
(longitud-lista '(1 2 3 4 5)) ;; Resultado esperado: 5

;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;Implementacion por listas

;; crear-FNC
;; La función crear-FNC crea una representación de una Forma Normal Conjuntiva
;; (FNC) con un número de variables y una expresión.
(define (crear-FNC numero-variables expresion)
  (list 'FNC numero-variables expresion))

; Casos de prueba
(crear-FNC 3 '(and (or (variable 1) (variable 2)) (or (not (variable 3)))))
;; Resultado esperado: (FNC 3 '(and (or (variable 1) (variable 2)) (or (not (variable 3)))))

(crear-FNC 2 '(and (or (variable 1) (variable 2))))
;; Resultado esperado: (FNC 2 '(and (or (variable 1) (variable 2)))


;; crear-expresion-final
;; La función crear-expresion-final crea una expresión final a partir de una cláusula dada.
;; Una expresión final es aquella que solo tiene una clausula, es decir que no tiene conjunciónes que
;; alarguen la expresion
(define (crear-expresion-final clausula)
  (list 'expresion clausula))

; Casos de prueba
(crear-expresion-final '(clausula (literal (variable 1)) 'or (clausula (literal (variable 2)))))
;; Resultado esperado: (expresion (clausula (literal (variable 1)) 'or (clausula (literal (variable 2))))))

(crear-expresion-final '(clausula (literal (variable 3)) 'or (clausula (literal (variable 4)))))
;; Resultado esperado: (expresion (clausula (literal (variable 3)) 'or (clausula (literal (variable 4))))))


;; crear-expresion
;; La función crear-expresion crea una expresión a partir de una cláusula, una conjunción y otra expresión.
;; Una expresión de este tipo es aquella que contiene una conjunción, por lo cual despues de ella se llama
;; recursivamente para crear una nueva expresión, que puede ser final o no.
(define (crear-expresion clausula conjuncion expresion)
  (list 'expresion clausula conjuncion expresion)  )

; Casos de prueba
(crear-expresion '(clausula (literal (variable 1))) 'and '(expresion-final (literal (variable -1))))
; Resultado esperado: (expresion (clausula (literal (variable 1))) and (expresion-final (literal (variable -1)

(crear-expresion '(clausula (literal (variable 1))) 'and '(expresion (literal (variable -1)) 'or '(expresion-final (literal(variable 2)))))
; Resultado esperado: (literal(variable 2)))))
;(expresion
; (clausula (literal (variable 1)))
; and
; (expresion
;  (literal (variable -1))
;  'or
;  '(expresion-final (literal (variable 2)))))



; crear-clausula-final
;; La función crear-clausula-final crea una cláusula final a partir de un literal.
;; Una clausula final es aquella que no contiene ninguna disyunción, y por ende,
;; finaliza su rama de la expresión.
(define (crear-clausula-final literal)
  (list 'clausula literal))

; Casos de prueba
(crear-clausula-final '(literal (variable 1)))
;Resultado esperado: (clausula (literal (variable 1)))

(crear-clausula-final '(literal (variable 2)))
;Resultado esperado: (clausula (literal (variable 2)))


; crear-clausula
;; Descripción: La función crear-clausula crea una cláusula a partir de un literal,
;; un operador de disyunción ("or"), y otra cláusula. Esta es aquella que si tiene una
;; disyunción, por lo cual se llama recursivamente para generar una nueva clausula
;; que puede o no ser una final.
(define (crear-clausula literal disyuncion clausula)
  (list 'clausula literal disyuncion clausula))

; Casos de prueba
(crear-clausula '(literal (variable 3)) 'or '(clausula-final (literal (variable 4))))
; Resultado esperado: '(clausula-final (literal (variable 4))))
;(clausula
; (literal (variable 3))
; or
; (clausula-final (literal (variable 4))))

(crear-clausula '(literal (variable 5)) 'or '(clausula-final (literal (variable 6))))
; Resultado esperado: (clausula
; (literal (variable 5))
; or
; (clausula-final (literal (variable 6))))

; crear-literal
;;La función crear-literal crea un literal a partir de una variable.
(define (crear-literal variable)
  (list 'literal variable))

; Casos prueba
(crear-literal '(variable 1))
; Resultado esperado: (literal (variable 1))
(crear-literal '(variable 2))
; Resultado esperado: (literal (variable 2))

; crear-variable
;;  La función crear-variable crea una representación de una variable a partir de un número.
(define (crear-variable num)
  (list 'variable num))
; Casos de prueba
(crear-variable 1)
; Resultado esperado: (variable 1)
(crear-variable 2)
; Resultado esperado: (variable 2)

;crear-conjuncion:
;;La función crear-conjuncion devuelve el operador de conjunción "and".
(define (crear-conjuncion)
  'and)
; Caso prueba
(crear-conjuncion)

;crear-disyuncion:
; La función crear-disyuncion devuelve el operador de disyunción "or".
(define (crear-disyuncion)
  'or)
; Caso prueba
(crear-disyuncion)

;Casos prueba utilizando todas las funciones a la vez
(define expresion-FNC-1 ; 1 and 2 and 3
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

;Resultado esperado:
; (FNC 3 (expresion (clausula (literal (variable 1))) and
; (expresion (clausula (literal (variable 2))) and
; (expresion (clausula (literal (variable 3)))))))

(define expresion-FNC-2 ; 1 and 2 and (3 or 4)
  (crear-FNC 4
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

;Resultado esperado:
; (FNC 4 (expresion (clausula (literal (variable 1))) and
; (expresion (clausula (literal (variable 2))) and
; (expresion (clausula (literal (variable 3)) or
; (clausula (literal (variable 4))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; es-FNC?
;  La función es-FNC? verifica si la entrada es una
; representación válida de una Forma Normal Conjuntiva (FNC).
; Un FNC valido es una lista de longitud 3, cuya cabeza es el
; texto 'FNC y cuyo segundo elemento es un número.

(define (es-FNC? FNC)
  (and (list? FNC)
       (= (longitud-lista FNC) 3)
       (eq? (car FNC) 'FNC)
       (number? (cadr FNC))))

;Casos de prueba (Definidas unas lineas arriba)
(es-FNC? expresion-FNC-1) ; resultado #t
(es-FNC? expresion-FNC-2) ; resultado #t


;es-expresion-final?:
;; La función es-expresion-final? verifica si la entrada es una expresión final válida.
;; Una expresion final valida es una lista de dos elementos, el primero es la cadena 'expresion
(define (es-expresion-final? expresion)
  (and (list? expresion) (eq? 'expresion (car expresion)) (= (longitud-lista expresion) 2)))

;Casos de prueba
(define exp-fin-1
  (crear-expresion-final
   (crear-clausula
    (crear-literal
     (crear-variable 3))
    (crear-disyuncion)
    (crear-clausula-final
     (crear-literal
      (crear-variable 4))))))

(define exp-no-fin-1
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
       (crear-variable 4)))))))

(es-expresion-final? exp-fin-1) ; resultado #t

(es-expresion-final? exp-no-fin-1) ; resultado #f pues una expresion final recibe un solo parametro
; mientras esta recibe 3 (es una expresion no final)

; es-expresion?
;; La función es-expresion? verifica si la entrada es una expresión válida.
;; Una expresion no final valida es una lista cuyo primer elemento es la cadena 'expresion
;; y su tercer elemento es una conjuncion (cadena 'and)
(define (es-expresion? expresion)
  (and (list? expresion)
       (eq? 'expresion (car expresion))
       (eq? 'and (caddr expresion))))

; Casos de prueba (exp-no-fin-1 esta definido mas arriba)

(define exp-no-fin-2
  (crear-expresion
   (crear-clausula
     (crear-literal
      (crear-variable 3))
     (crear-disyuncion)
     (crear-clausula-final
      (crear-literal
       (crear-variable 4))))
   (crear-conjuncion)
   (crear-expresion-final
    (crear-clausula
     (crear-literal
      (crear-variable 3))
     (crear-disyuncion)
     (crear-clausula-final
      (crear-literal
       (crear-variable 4)))))))

(es-expresion? exp-no-fin-1) ; resultado #t
(es-expresion? exp-no-fin-2) ; resultado #t

; es-clausula-final?:
;;La función es-clausula-final? verifica si la entrada es una cláusula final válida.
;; Una clausula final valida es una lista de dos elementos cuyo primer elemento es
;; una cadena que dice ('clausula).
(define (es-clausula-final? clausula)
  (and (list? clausula) (eq? 'clausula (car clausula))(= (longitud-lista clausula) 2)))

; Casos de prueba
(define clau-fin-1
  (crear-clausula-final
   (crear-literal
    (crear-variable 4))))

(define clau-no-fin-1
  (crear-clausula
   (crear-literal
    (crear-variable 3))
   (crear-disyuncion)
   (crear-clausula-final
    (crear-literal
     (crear-variable 4)))))

(es-clausula-final? clau-fin-1) ;resultado #t
(es-clausula-final? clau-no-fin-1) ;resultado #f

;es-clausula?
; La función es-clausula? verifica si la entrada es una cláusula válida.
;; Una clausula no final valida es una lista de elementos cuyo primer elemento es
;; la cadena ('clausula) y su tercer elemento es una disyuncion ('or)
(define (es-clausula? clausula)
  (and (list? clausula)
       (eq? 'clausula (car clausula))
       (eq? 'or (caddr clausula))))

(es-clausula? exp-no-fin-1) ; resultado #f; una expresion no es una clausula
(es-clausula? clau-no-fin-1) ; resultado # t


;es-literal?
;;La función es-literal? verifica si la entrada es un literal válido.
;;Un literal valida es una lista cuyo primer elemento es una cadena
;; que dice ('literal)
(define (es-literal? literal)
  (and (list? literal) (eq? 'literal (car literal))))

; Casos de prueba
(define lit-1
  (crear-literal
     (crear-variable 4)))

(es-literal? lit-1) ; resultado #t
(es-literal? exp-no-fin-1) ; resultado #f, pues se le esta ingresando una expresión

;es-variable?
;; La función es-variable? verifica si la entrada es una variable válida.
;; Una variable valida es una lista cuyo primer elemento es la cadena ('variable)
;; y su segundo elemento es un número.
(define (es-variable? variable)
  (and (list? variable) (eq? 'variable (car variable)) (number? (cadr variable))))

; Casos de prueba
(define var-1
  (crear-variable 2))

(define var-2
  (crear-variable 'n))

(es-variable? var-1) ; resultado #t
(es-variable? var-2) ; resultado #f, pues se le esta ingresando un simbolo

(define (es-conjuncion? expr)
  (eq? expr 'and))

;Casos de prueba
(es-conjuncion? 'and) ;resultado #t
(es-conjuncion? 'or) ;resultado #f

(define (es-disyuncion? expr)
  (eq? expr 'or))

;Casos de prueba
(es-disyuncion? 'and) ;resultado #f
(es-disyuncion? 'or) ;resultado #t

(define (es-numero? expr)
  (number? expr))

;Casos de prueba
(es-numero? 3) ;resultado #t
(es-numero? 'a) ;resultado #f

;obtener-numero-variables
;;Funcion que extrae el numero de variables presentes en una expresión FNC.
;;Cumple su objetivo accediendo al segundo elemento de una FNC valida.
(define (obtener-numero-variables FNC)
  (if (es-FNC? FNC)
      (cadr FNC)
      (eopl:error 'obtener-numero-variables "Expecting expression, given ~s" FNC)))

(obtener-numero-variables expresion-FNC-1)
(obtener-numero-variables expresion-FNC-2)


;obtener-expresion
;;Funcion que extrae la expresión en una expresión FNC.
;;Esto lo logra accediendo al tercer elemento de una FNC valida.
(define (obtener-expresion FNC)
  (if (es-FNC? FNC)
      (caddr FNC)
      (eopl:error 'obtener-expresion "Expecting BNF expression, given ~s" FNC)))

(obtener-expresion expresion-FNC-1)
(obtener-expresion expresion-FNC-2)


;obtener-expresion-interna
;;Funcion que extrae la expresión interna dentro en una expresión.
;;Para esto se debe acceder al cuarto elemento de una expresion no final valida.
(define (obtener-expresion-interna expresion)
  (cadddr expresion))

(obtener-expresion-interna (obtener-expresion expresion-FNC-1))
(obtener-expresion-interna (obtener-expresion expresion-FNC-2))

;obtener-clausula
;;Funcion que extrae la clausula en una expresión.
;;Para esto se debe acceder al segundo elemento de una expresion final o no final valida.
(define (obtener-clausula expresion)
  (if (or (es-expresion-final? expresion) (es-expresion? expresion))
      (cadr expresion)
      (eopl:error 'obtener-clausula "Expecting expression, given ~s" expresion)))

(obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-1)))
(obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-2)))

;obtener-literal
;;Funcion que extrae el literal de una clausula.
;;Su funcionamiento es extraer el segundo elemento de una clausula valida, ya sea final o no final.
(define (obtener-literal clausula)
  (if (or (es-clausula-final? clausula) (es-clausula? clausula))
      (cadr clausula)
      (eopl:error 'obtener-literal "Expecting clausula, given ~s" clausula)))

(obtener-literal (obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-1))))
(obtener-literal (obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-2))))

;obtener-variable
;;Funcion que extrae la variable de un literal.
;;Funciona accediendo al segundo elemento de un literal previamente validado.
(define (obtener-variable literal)
  (if (es-literal? literal)
      (cadr literal)
      (eopl:error 'obtener-variable "Expecting literal, given ~s" literal)))

(obtener-variable (obtener-literal (obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-1)))))
(obtener-variable (obtener-literal (obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-2)))))

;obtener-numero
;;Funcion que extrae el numero de una variable.
;;Funciona accediendo al segundo elemento de una variable, cuya validez es verificada previamente.
(define (obtener-numero variable)
  (if (es-variable? variable)
      (cadr variable)
      (eopl:error 'obtener-numero "Expecting variable, given ~s" variable)))

(obtener-numero (obtener-variable (obtener-literal (obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-1))))))
(obtener-numero (obtener-variable (obtener-literal (obtener-clausula (obtener-expresion-interna (obtener-expresion expresion-FNC-2))))))

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


