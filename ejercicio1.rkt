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
;; Implementacion de ambientes (codigo de clase)
;; empty-env
;; Esta función crea un entorno vacío en forma de una lista.
;; Se emplea para inicializar un entorno vacío que se utilizará
;; posteriormente para mantener asociaciones entre variables y
;; valores en un contexto dado.
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env
;; Esta función extiende un entorno existente agregando una nueva
;; asociación entre una variable (var) y su valor (val).
;; se utiliza para crear un nuevo entorno que contiene una
;; extensión del entorno original. Esta extensión permite que una
;; variable esté vinculada a un valor dentro del entorno resultante.
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;; apply-env
;; Esta función busca una variable (search-var) en un entorno dado
;; (env) y devuelve su valor asociado si se encuentra. Si la variable
;; no se encuentra en el entorno, genera un error.
;; Se emplea para buscar y recuperar el valor asociado a una variable
;; en un entorno. Esta función es útil en el contexto de la evaluación
;; de expresiones o la resolución de variables dentro de un programa.
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
;; negar
;; La función negar toma un valor booleano como entrada y devuelve el valor booleano
;; negado. Si el valor de entrada es #t, la función devuelve #f, y si el valor de entrada
;; es #f, la función devuelve #t. Esta función se emplea para cambiar el valor de verdad
;; de una variable booleana.
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
       (or (eq? 'and (caddr expresion))
           (eq? 'or (caddr expresion)))))

(define (es-clausula-final? clausula)
  (and (list? clausula) (eq? 'clausula-final (car clausula))))

(define (es-clausula? clausula)
  (and (list? clausula)
       (eq? 'clausula (car clausula))
       (or  (eq? 'and (caddr clausula))
            (eq? 'or (caddr clausula)))))

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

(define expresion-sat-doble ; (1 or -1) and 2
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

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::;
;; EVALUARSAT
;; Esta función toma una expresión FNC y
;; busca una asignación de variables que satisfaga la expresión,
;; si se encuentra una asignación satisfactoria devuelve una
;; lista que indica que la FNC es satisfactible junto con la
;; asignación que lo satisface. Si no se encuentra ninguna
;; asignación satisfactoria devuelve una lista que indica que
;; la FNC es insatisfactible junto con una lista vacía.
;; Se emplea para determinar si una expresión FNC dada es satisfactible
;; y en caso afirmativo, proporcionar una asignación de variables que satisfaga la expresión.
(define (EVALUARSAT FNC)
  (let ((solucion (explorar-solucion FNC (obtener-numero-variables FNC) '())))
    (if (null? solucion)
        (list 'insatisfactible '())
        (list 'satisfactible solucion))))

;; Esta función es una función auxiliar utilizada por EVALUARSAT.
;; Explora recursivamente posibles asignaciones de variables para
;; verificar si alguna de ellas satisface la FNC dada. Comienza con
;; una asignación actual y prueba todas las posibles combinaciones
;; de valores de verdad para las variables. Devuelve la asignación
;; si encuentra una que satisface la FNC o una lista vacía si no la encuentra.
;; Se emplea internamente en la función EVALUARSAT para buscar asignaciones que satisfagan la FNC.
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
              solucion-false)))))

;; crear-ambiente
;; Esta función crea un ambiente de variables a partir de una asignación
;; de variables dada. Toma la FNC (FNC) y una asignación de variables (asignacion)
;; y crea un entorno (ambiente) en el que las variables están vinculadas a sus valores
;; correspondientes según la asignación.
;; Se utiliza para generar un entorno que contiene las asociaciones entre variables
;; y valores según una asignación específica, esto es util para la evaluación posterior de la FNC.
(define (crear-ambiente FNC asignacion)

  ;; crear-ambiente-aux
  ;; La función crear-ambiente-aux crea un ambiente de variables
  ;; a partir de una asignación de valores. Toma tres argumentos:
  ;; vals, una lista de variables; asignacion, una lista de valores
  ;; correspondientes a esas variables; y env, un ambiente de variables existente.
  ;; La función genera un nuevo ambiente donde cada variable en vals se asocia
  ;; a su valor correspondiente en asignacion, basándose en el ambiente previo env.
  ;; Luego, se llama de manera recursiva para procesar las variables restantes
  ;; en vals y sus valores en asignacion. Se utiliza en la satisfactibilidad
  ;; de expresiones FNC para crear ambientes de variables basados en asignaciones específicas.
  (define (crear-ambiente-aux vals asignacion env)
    (if (equal? (longitud-lista vals) 0)
        env
        (letrec ((new-env (extend-env (car vals) (car asignacion) env)))
          (crear-ambiente-aux (cdr vals) (cdr asignacion) new-env))))

  (let ((vals (variables-FNC FNC)))
    (crear-ambiente-aux vals asignacion (empty-env))))

;; variables-FNC
;; La función variables-FNC se encarga de extraer todas las variables presentes
;; en una expresión FNC (Forma Normal Conjuntiva) dada. Esta función recorre la
;; estructura de la FNC y obtiene todas las variables presentes, sin diferenciar
;; entre si están negadas (son números positivos) o no. En resumen, toma una expresión
;; FNC como entrada y devuelve una lista que contiene todas las variables presentes
;; en esa expresión, sin elementos repetidos.
;; Esta función es útil en el contexto de la satisfactibilidad de expresiones lógicas
;; donde es necesario conocer todas las variables involucradas en la expresión para
;; evaluarla adecuadamente y determinar su satisfactibilidad.
(define (variables-FNC FNC)

  ;; up
  ;; La función up toma una lista L como entrada y elimina todos los
  ;; niveles internos de paréntesis de la lista, dejando solo los
  ;; elementos en una lista plana. Esta función se emplea para
  ;; simplificar una estructura de lista anidada, extrayendo todos
  ;; los elementos de las sublistas y presentándolos en una lista única y plana.
  (define (up L)

    ;; up-aux
    ;; La función interna up-aux se utiliza en la función principal up
    ;; y desempeña un papel crucial en la eliminación de los niveles
    ;; internos de paréntesis en una lista. up-aux toma dos listas como
    ;; entrada, l1 y l2. Su principal función es recorrer l1 y, en caso
    ;; de encontrar elementos que son sublistas, los descomprime y agrega
    ;; sus elementos a l2, creando así una lista plana sin niveles internos
    ;; de paréntesis. La función up-aux se utiliza para procesar y aplanar
    ;; las sublistas anidadas dentro de la lista original L que se pasa como
    ;; argumento a la función up.
    ;; La función principal up usa up-aux para realizar la eliminación de los niveles internos de paréntesis en la lista L.
    (define (up-aux l1 l2)
      (cond
        ((null? l1) l2)
        (else (cons (car l1) (up-aux (cdr l1) l2)))))
    (cond
      ((null? L) '())
      ((list? (car L)) (up-aux (up (car L)) (up (cdr L))))
      (else (cons (car L) (up (cdr L))))))

  ;; limpiar-lista
  ;; La función limpiar-lista recibe una lista de números
  ;; realiza varias operaciones en ella y devuelve una nueva
  ;; lista que contiene los valores absolutos de los números
  ;; en la lista de entrada, eliminando cualquier número repetido.
  ;; Recibe una lista de números, primero calcula el valor absoluto
  ;; de cada número en la lista de entrada utilizando la función
  ;; auxiliar valor-absoluto, luego utiliza la función eliminar-repetidos
  ;; para eliminar los números duplicados en la lista resultante y devuelve
  ;; una nueva lista que contiene los valores absolutos de los números en la
  ;; lista de entrada, sin elementos repetidos.
  ;; Esta función se emplea para sacar todas las variables sin diferenciar entre
  ;; si estan negadas (son numero positivo) o no.
  (define (limpiar-lista lista)

    ;; valor-absoluto
    ;; Esta función toma un número num y calcula su valor absoluto,
    ;; devuelve el número positivo correspondiente al valor absoluto del número negativo.
    (define (valor-absoluto num)
      (if (< num 0) (- num) num))

    ;; mapping
    ;; Esta función aplica la función funcion a cada elemento de
    ;; la lista lista. Devuelve una nueva lista con los resultados
    ;; de aplicar la función a cada elemento de la lista original.
    (define (mapping funcion lista)
      (if (null? lista)
          '()
          (cons (funcion (car lista)) (mapping funcion (cdr lista)))))

    ;; emilimar-repetidos
    ;; Esta función toma una lista lst y elimina los elementos repetidos,
    ;; devuelve una nueva lista sin repetidos.
    (define (eliminar-repetidos lst)
      (cond
        ((null? lst) '())
        (else
         (if (elemento-repetido? (car lst) (cdr lst))
             (eliminar-repetidos (cdr lst))
             (cons (car lst) (eliminar-repetidos (cdr lst)))))))

    ;; elemento-repetido?
    ;; Esta función verifica si un elemento dado elemento está repetido
    ;; en una lista lista. Devuelve #t si el elemento está repetido y #f
    ;; en caso contrario.
    (define (elemento-repetido? elemento lista)
      (cond
        ((null? lista) #f)
        ((equal? elemento (car lista)) #t)
        (else (elemento-repetido? elemento (cdr lista)))))

    (let ((lista-abs (mapping valor-absoluto lista)))
      (eliminar-repetidos lista-abs)))

  ;; La función variables-FNC-aux se utiliza para extraer
  ;; todas las variables presentes en una expresión FNC dada.
  ;; Esta función toma una expresión como entrada y si es
  ;; una expresión final devuelve una lista que contiene
  ;; la variable presente en esa expresión. Si la expresión
  ;; no es final (es una expresión compuesta),
  ;; recursivamente se extraen las variables de cada subexpresión
  ;; interna y se combinan en una lista.
  (define (variables-FNC-aux expresion)
    (if (es-expresion-final? expresion)
        (list (obtener-numero (obtener-variable (obtener-literal (obtener-clausula expresion)))))
        (list (obtener-numero (obtener-variable (obtener-literal (obtener-clausula expresion))))
              (variables-FNC-aux (obtener-expresion-interna expresion)))))

  (cond
    ((es-FNC? FNC)
     (letrec ((expresion (obtener-expresion FNC)))
       (limpiar-lista (up (variables-FNC-aux expresion)))
       ))
    (else (eopl:error 'variables-FNC "Expecting FNC, given ~s" FNC))))

;; evaluar-FNC
;; La función evaluar-FNC se encarga de evaluar una
;; expresión FNC dada respecto a una asignación de variables específica.
;; Para lograr esto, primero obtiene la expresión FNC
;; y crea un entorno (ambiente) de variables basado en
;; la asignación proporcionada, luego utiliza este
;; ambiente para evaluar la expresión FNC. Esta función
;; es empleada para determinar si una asignación de variables
;; satisface una FNC dada, es decir, si hace que la FNC sea verdadera o falsa.
(define (evaluar-FNC FNC asignacion)
  (let ((expresion (obtener-expresion FNC))
        (env (crear-ambiente FNC asignacion)))
    (evaluar-expresion expresion asignacion env)))

;; evaluar-expresion
;; Esta función se utiliza para evaluar una expresión en el contexto
;; de una asignación de variables y un ambiente dado, puede manejar
;; expresiones que son finales (terminales) o compuestas por conjunciones
;; y disyunciones, evalúa la expresión y devuelve el resultado booleano de la evaluación.
;; Se emplea en la evaluación de satisfactibilidad (SAT) de expresiones FNC.
;; Puede evaluar una expresión FNC y determinar si es verdadera o falsa
;; en función de una asignación de variables y un ambiente específicos.
(define (evaluar-expresion expresion asignacion env)
  (cond
    ((es-expresion-final? expresion)
     (evaluar-clausula (obtener-clausula expresion) asignacion env)
     )
    ((es-expresion? expresion)
     (cond
       ((es-conjuncion? (caddr expresion))
        (
         and (evaluar-clausula (obtener-clausula expresion) asignacion env)
             (evaluar-expresion (obtener-expresion-interna expresion) asignacion env)
             ))
       ((es-disyuncion? (caddr expresion))
        (
         or (evaluar-clausula (obtener-clausula expresion) asignacion env)
            (evaluar-expresion (obtener-expresion-interna expresion) asignacion env)
            ))))
    (else
     (eopl:error 'evaluar-expresion "Entrada no válida: se esperaba una expresión, se recibió ~s" expresion))))

;; evaluar-clausula
;; La función evaluar-clausula se utiliza para evaluar una cláusula en el
;; contexto de la satisfactibilidad de una expresión en FNC.
;; Esta función toma una cláusula (clausula), una asignación de variables (asignacion)
;; y un ambiente (env) como entrada y determina si la cláusula es verdadera o falsa en
;; función de la asignación de variables y el ambiente proporcionados.
;; La función evalúa una cláusula en el contexto de una FNC, comprueba si la
;; cláusula es verdadera o falsa en función de los valores de verdad de los literales
;; que la componen y la asignación de variables proporcionada.
;; Se emplea en el proceso de evaluación de una FNC para verificar si una
;; cláusula específica es satisfactible o no y contribuye a la evaluación global de la FNC
;; y a determinar si una asignación satisface la expresión FNC en su conjunto.
(define (evaluar-clausula clausula asignacion env)
  (cond
    ((es-clausula-final? clausula)
     (evaluar-literal (obtener-literal clausula) asignacion env))
    ((es-clausula? clausula)
     (cond
       ((es-conjuncion? (caddr clausula))
        (and (evaluar-literal (obtener-literal clausula) asignacion env)
             (evaluar-clausula (obtener-clausula clausula) asignacion env)))
       ((es-disyuncion? (caddr clausula))
        (or (evaluar-literal (obtener-literal clausula) asignacion env)
            (evaluar-clausula (obtener-clausula clausula) asignacion env))))
     )
    (else
     (eopl:error 'evaluar-clausula "Entrada no válida: se esperaba una cláusula, se recibió ~s" clausula)))
  )

;; evaluar-literal
;; La función evaluar-literal se utiliza para evaluar el valor de una variable
;; literal dentro del contexto de un ambiente dado, toma como entrada un literal
;; (literal), una asignación de variables (asignacion) y un ambiente de variables (env).
;; La función primero obtiene la variable del literal y su valor correspondiente, luego
;; verifica si la variable es negativa o positiva y utiliza el ambiente (env) para
;; recuperar el valor apropiado de la variable. Si la variable es negativa se negará
;; su valor en el ambiente. Se emplea en el contexto de la evaluación de expresiones
;; FNC para determinar el valor de verdad de una variable literal en función de una
;; asignación y un ambiente dados.
(define (evaluar-literal literal asignacion env)
  (let ((variable (obtener-variable literal)))
    (let ((valor (obtener-numero variable)))
      (if (es-variable? variable)
          (if (< valor 0)
              (negar (apply-env env (* -1 valor)))
              (apply-env env valor))
          (eopl:error 'evaluar-literal "Entrada no válida: se esperaba una variable, se recibió ~s" variable)))))

;; EVALUARSAT: Ejemplos de uso
(EVALUARSAT (crear-FNC 1 ;; x
                       (crear-expresion-final
                        (crear-clausula-final
                         (crear-literal
                          (crear-variable 3)))))) ;; (satisfactible (#t))
(EVALUARSAT (crear-FNC 1 ;; -x
                       (crear-expresion-final
                        (crear-clausula-final
                         (crear-literal
                          (crear-variable -3)))))) ;; (satisfactible (#f))
(EVALUARSAT (crear-FNC 1;; -x and x
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
                        ))) ;; (insatisfactible ())
(EVALUARSAT (crear-FNC 2 ;; x and y
                       (crear-expresion
                        (crear-clausula-final
                         (crear-literal
                          (crear-variable 2)))

                        (crear-conjuncion)

                        (crear-expresion-final
                         (crear-clausula-final
                          (crear-literal
                           (crear-variable 1))))))) ;; (satisfactible (#t #t))
(EVALUARSAT (crear-FNC 2 ;; x and -y
                       (crear-expresion
                        (crear-clausula-final
                         (crear-literal
                          (crear-variable 2)))

                        (crear-conjuncion)

                        (crear-expresion-final
                         (crear-clausula-final
                          (crear-literal
                           (crear-variable -1))))))) ;; (satisfactible (#t #f))
(EVALUARSAT (crear-FNC 3 ;; -x and -y and -z
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
                            (crear-variable -3)))))))) ;; (satisfactible (#f #f #f))

;; Gramatica BNF
;; <FNC> ::= 'FNC <numero-variables> <expresion>
;; <numero-variables> ::= <int>
;; <expresion> ::= <clausula> | <clausula> <conjuncion> <expresion>
;; <clausula> ::= <literal> | <literal> <disyunción> <clausula>
;; <literal> ::= <variable>
;; <variable> ::= <int>
;; <conjuncion> ::= 'and
;; <disyunción> ::= 'or

(EVALUARSAT (crear-FNC 4 ; 1 and (2 or 3)
                       (crear-expresion
                        (crear-clausula-final
                         (crear-literal
                          (crear-variable 1)))

                        (crear-conjuncion)

                        (crear-expresion-final

                         (crear-clausula
                          (crear-literal
                           (crear-variable 2))

                          (crear-disyuncion)

                          (crear-clausula-final
                           (crear-literal
                            (crear-variable 3))))))))