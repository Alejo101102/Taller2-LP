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


;; Gramatica BNF
;; <FNC> ::= 'FNC <numero-variables> <expresion>
;; <numero-variables> ::= <int>
;; <expresion> ::= <clausula> | <clausula> <conjuncion> <expresion>
;; <conjuncion> ::= 'and
;; <clausula> ::= <literal> | <literal> <disyunción> <clausula>
;; <disyunción> ::= 'or
;; <literal> ::= <variable>
;; <variable> ::= <int>


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



;; cons-end
;; Esta función agrega un elemento (elemento) al final de una lista (lista) dada.
;; Se utiliza para construir nuevas listas agregando un elemento
;; al final de una lista existente. Puede ser útil en varias operaciones
;; de manipulación de listas y estructuras de datos en Racket.
(define (cons-end lista elemento)
  (cond
    ((null? lista) elemento)
    (else (cons (car lista)
                (cons-end (cdr lista) elemento)))))

; Caso de prueba 1: Agregar un elemento a una lista vacía
(cons-end '() 42) ;; '(42).
; Caso de prueba 2: Agregar un elemento al final de una lista existente
(cons-end '(1 2 3) 4) ;; '(1 2 3 4).




;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;Implementacion por listas


;; crear-FNC
;; La función crear-FNC crea una representación de una Forma Normal Conjuntiva
;; (FNC) con un número de variables y una expresión.
(define (crear-FNC numero-variables expresion)
  (list 'FNC numero-variables expresion))



;; crear-expresion-final
;; La función crear-expresion-final crea una expresión final a partir de una cláusula dada.
;; Una expresión final es aquella que solo tiene una clausula, es decir que no tiene conjunciónes que
;; alarguen la expresion
(define (crear-expresion-final clausula)
  (list 'expresion clausula))



;; crear-expresion
;; La función crear-expresion crea una expresión a partir de una cláusula, una conjunción y otra expresión.
;; Una expresión de este tipo es aquella que contiene una conjunción, por lo cual despues de ella se llama
;; recursivamente para crear una nueva expresión, que puede ser final o no.
(define (crear-expresion clausula conjuncion expresion)
  (list 'expresion clausula conjuncion expresion)  )



; crear-clausula-final
;; La función crear-clausula-final crea una cláusula final a partir de un literal.
;; Una clausula final es aquella que no contiene ninguna disyunción, y por ende,
;; finaliza su rama de la expresión.
(define (crear-clausula-final literal)
  (list 'clausula literal))



; crear-clausula
;; Descripción: La función crear-clausula crea una cláusula a partir de un literal,
;; un operador de disyunción ("or"), y otra cláusula. Esta es aquella que si tiene una
;; disyunción, por lo cual se llama recursivamente para generar una nueva clausula
;; que puede o no ser una final.
(define (crear-clausula literal disyuncion clausula)
  (list 'clausula literal disyuncion clausula))




; crear-literal
;;La función crear-literal crea un literal a partir de una variable.
(define (crear-literal variable)
  (list 'literal variable))



; crear-variable
;;  La función crear-variable crea una representación de una variable a partir de un número.
(define (crear-variable num)
  (list 'variable num))



;crear-conjuncion:
;;La función crear-conjuncion devuelve el operador de conjunción "and".
(define (crear-conjuncion)
  'and)



;crear-disyuncion:
; La función crear-disyuncion devuelve el operador de disyunción "or".
(define (crear-disyuncion)
  'or)



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




;es-expresion-final?:
;; La función es-expresion-final? verifica si la entrada es una expresión final válida.
;; Una expresion final valida es una lista de dos elementos, el primero es la cadena 'expresion
(define (es-expresion-final? expresion)
  (and (list? expresion) (eq? 'expresion (car expresion)) (= (longitud-lista expresion) 2)))



; es-expresion?
;; La función es-expresion? verifica si la entrada es una expresión válida.
;; Una expresion no final valida es una lista cuyo primer elemento es la cadena 'expresion
;; y su tercer elemento es una conjuncion (cadena 'and)
(define (es-expresion? expresion)
  (and (list? expresion)
       (eq? 'expresion (car expresion))
       (eq? 'and (caddr expresion))))



; es-clausula-final?:
;;La función es-clausula-final? verifica si la entrada es una cláusula final válida.
;; Una clausula final valida es una lista de dos elementos cuyo primer elemento es
;; una cadena que dice ('clausula).
(define (es-clausula-final? clausula)
  (and (list? clausula) (eq? 'clausula (car clausula))(= (longitud-lista clausula) 2)))



;es-clausula?
; La función es-clausula? verifica si la entrada es una cláusula válida.
;; Una clausula no final valida es una lista de elementos cuyo primer elemento es
;; la cadena ('clausula) y su tercer elemento es una disyuncion ('or)
(define (es-clausula? clausula)
  (and (list? clausula)
       (eq? 'clausula (car clausula))
       (eq? 'or (caddr clausula))))



;es-literal?
;;La función es-literal? verifica si la entrada es un literal válido.
;;Un literal valida es una lista cuyo primer elemento es una cadena
;; que dice ('literal)
(define (es-literal? literal)
  (and (list? literal) (eq? 'literal (car literal))))




;es-variable?
;; La función es-variable? verifica si la entrada es una variable válida.
;; Una variable valida es una lista cuyo primer elemento es la cadena ('variable)
;; y su segundo elemento es un número.
(define (es-variable? variable)
  (and (list? variable) (eq? 'variable (car variable)) (number? (cadr variable))))



;es-conjuncion?
;; La función examina si una expresión que se ingresa como parametro es igual a
;; la cadena 'and, y retorna su valor de verdad.
(define (es-conjuncion? expr)
  (eq? expr 'and))



;es-disyuncion?
;; La función examina si una expresión que se ingresa como parametro es igual a
;; la cadena 'or, y retorna su valor de verdad.
(define (es-disyuncion? expr)
  (eq? expr 'or))


;es-numero?
;; La función examina si una expresión que se ingresa como parametro es
;; un número, y reotrna su respectivo valor de verdad.
(define (es-numero? expr)
  (number? expr))


;obtener-numero-variables
;;Funcion que extrae el numero de variables presentes en una expresión FNC.
;;Cumple su objetivo accediendo al segundo elemento de una FNC valida.
(define (obtener-numero-variables FNC)
  (if (es-FNC? FNC)
      (cadr FNC)
      (eopl:error 'obtener-numero-variables "Expecting expression, given ~s" FNC)))



;obtener-expresion
;;Funcion que extrae la expresión en una expresión FNC.
;;Esto lo logra accediendo al tercer elemento de una FNC valida.
(define (obtener-expresion FNC)
  (if (es-FNC? FNC)
      (caddr FNC)
      (eopl:error 'obtener-expresion "Expecting BNF expression, given ~s" FNC)))



;obtener-expresion-interna
;;Funcion que extrae la expresión interna dentro en una expresión.
;;Para esto se debe acceder al cuarto elemento de una expresion no final valida.
(define (obtener-expresion-interna expresion)
  (cadddr expresion))



;obtener-clausula
;;Funcion que extrae la clausula en una expresión.
;;Para esto se debe acceder al segundo elemento de una expresion final o no final valida.
(define (obtener-clausula expresion)
  (if (or (es-expresion-final? expresion) (es-expresion? expresion))
      (cadr expresion)
      (eopl:error 'obtener-clausula "Expecting expression, given ~s" expresion)))



;obtener-literal
;;Funcion que extrae el literal de una clausula.
;;Su funcionamiento es extraer el segundo elemento de una clausula valida, ya sea final o no final.
(define (obtener-literal clausula)
  (if (or (es-clausula-final? clausula) (es-clausula? clausula))
      (cadr clausula)
      (eopl:error 'obtener-literal "Expecting clausula, given ~s" clausula)))



;obtener-variable
;;Funcion que extrae la variable de un literal.
;;Funciona accediendo al segundo elemento de un literal previamente validado.
(define (obtener-variable literal)
  (if (es-literal? literal)
      (cadr literal)
      (eopl:error 'obtener-variable "Expecting literal, given ~s" literal)))



;obtener-numero
;;Funcion que extrae el numero de una variable.
;;Funciona accediendo al segundo elemento de una variable, cuya validez es verificada previamente.
(define (obtener-numero variable)
  (if (es-variable? variable)
      (cadr variable)
      (eopl:error 'obtener-numero "Expecting variable, given ~s" variable)))


;; NOTA: Los casos pruebas de las funciones pasadas se encuentran en el archivo ejercicio1


;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;


;; PARSEBNF
;; La función PARSEBNF toma una entrada en forma de lista y la convierte
;; en una representación interna de una expresión lógica en Forma Normal Conjuntiva (FNC).
;;
;; La función PARSEBNF procesa la entrada y la convierte en una estructura de datos interna
;; que representa la expresión en Forma Normal Conjuntiva (FNC). Puede manejar operadores lógicos
;; 'or', 'and', y 'not' en la entrada, y crea cláusulas y literales según sea necesario para
;; representar la expresión en FNC.
(define (PARSEBNF entrada)
  (cond
    ((number? entrada) (crear-literal (crear-variable entrada)))
    ((eq? (car entrada) 'or) (crear-expresion-final (crear-clausula-final (aplicar-operador 'or (cdr entrada)))))
    ((eq? (car entrada) 'and) (crear-expresion-final (crear-clausula-final (aplicar-operador 'and (cdr entrada)))))
    ((eq? (car entrada) 'not) (crear-expresion-final (crear-clausula-final (aplicar-not (cdr entrada)))))
    (else (eopl:error "Entrada no válida"))))


;; aplicar-not
;; La función aplicar-not toma una lista de elementos y aplica el operador lógico 'not' a estos elementos.
;;
;; La función aplicar-not procesa la lista de elementos y aplica el operador lógico 'not' a cada uno de ellos.
;; Si la lista de elementos tiene un solo elemento, se crea una estructura de datos que representa un literal
;; negado basado en ese elemento. Si la lista de elementos no tiene exactamente un elemento, se genera un error.
(define (aplicar-not elementos)
  (cond
    ((null? elementos) (eopl:error "Operador 'not' debe tener un argumento"))
    ((null? (cdr elementos)) (crear-literal (crear-variable (list 'not (PARSEBNF (car elementos))))))
    (else (eopl:error "Operador 'not' solo debe tener un argumento"))))
(define elemento-a-negar 42) ; Definimos un elemento, en este caso, un número.
(define resultado (aplicar-not (list elemento-a-negar)))
(display resultado)
(newline);;salto de línea
(define elemento-a-negar-2 25) ; Definimos un elemento, en este caso, un número.
(define resultado-2 (aplicar-not (list elemento-a-negar-2)))
(display resultado-2)
(newline);;salto de línea
(newline);;salto de línea


;; aplicar-operador
;; La función aplicar-operador toma un operador lógico y una lista de elementos y aplica el operador a esos elementos.
;;
;; La función aplicar-operador se utiliza en el proceso de análisis y transformación de expresiones lógicas en
;; Forma Normal Conjuntiva (FNC). Permite aplicar operadores lógicos 'or' y 'and' a una lista de elementos,
;; lo que es esencial para construir cláusulas y expresiones lógicas en FNC. Esta función es una parte fundamental
;; de la conversión de una expresión lógica en un formato adecuado para su procesamiento y evaluación posterior.

(define (aplicar-operador operador elementos)
  (cond
    ((null? elementos) (if (eq? operador 'or) 'F 'T))
    ((null? (cdr elementos)) (PARSEBNF (car elementos)))
    (else (crear-clausula (crear-literal (PARSEBNF (car elementos))) operador (aplicar-operador operador (cdr elementos))))))


(define elementos-para-or '(1 2 3)) ; Definimos una lista de elementos para aplicar el operador 'or'.

(define resultado-or (aplicar-operador 'or elementos-para-or))

(define elementos-para-and '(4 5 6)) ; Definimos una lista de elementos para aplicar el operador 'and'.

(define resultado-and (aplicar-operador 'and elementos-para-and))

(display resultado-or)
(newline)
(display resultado-and)
(newline)
(newline)
;;Se espera
#|
(clausula (literal (literal (variable 1))) or (clausula (literal (literal (variable 2))) or (literal (variable 3))))
(clausula (literal (literal (variable 4))) and (clausula (literal (literal (variable 5))) and (literal (variable 6))))
|#

; Primer ejemplo de uso PARSEBNF:
(define entrada-ejemplo '(and 1 (or 2 (not 3)) 5))
(define arbol-abstracto (PARSEBNF entrada-ejemplo))
(display arbol-abstracto)
(newline);;salto de línea
(newline);;salto de línea
;;Se espera
#|
(expresion (clausula (clausula (literal (literal (variable 1))) and (clausula (literal (expresion (clausula (clausula (literal (literal (variable 2)))
or (expresion (clausula (literal (variable (not (literal (variable 3))))))))))) and (literal (variable 5))))))
|#


; Segundo ejemplo de uso PARSEBNF:
(define segunda-entrada-ejemplo '(and 1 (or 2 3) 4 5))
(define arbol-abstracto-2 (PARSEBNF segunda-entrada-ejemplo))
(display arbol-abstracto-2)
(newline);;salto de línea
(newline);;salto de línea
;;Se espera
#|
(expresion (clausula (clausula (literal (literal (variable 1))) and (clausula (literal (expresion (clausula (clausula (literal (literal (variable 2)))
or (literal (variable 3)))))) and (clausula (literal (literal (variable 4))) and (literal (variable 5)))))))
|#
