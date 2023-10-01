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

;;Constructores

(define (conjuncion)
  'and)

(define (disyuncion)
  'or)

(define or-expression
  (lambda (a b c)
    (list 'or-expression (a b c))))

(define and-expression
  (lambda (q w e)
    (list 'and-expression (q w e))))

;;Predicados

(define or-expression?
  (lambda (disyuncion)
    (eq? 'or-expression (cadr disyuncion))))

(define and-expression?
  (lambda (conjuncion)
    (eq? 'or-expression (cadr conjuncion))))

;; Demostración

(define or-expression->or
  (lambda (sym)
    (cadr sym)))

(define or-expression->left
  (lambda (sym)
    (car sym)))

(define or-expression->right
  (lambda (sym)
    (caddr sym)))

(define and-expression->and
  (lambda (sym)
    (cadr sym)))

(define and-expression->left
  (lambda (sym)
    (car sym)))

(define and-expression->right
  (lambda (sym)
    (caddr sym)))

;;Programación

(define PARSEBNF
  (lambda (parse)
    (cond
      [(or-expression? parse)
       (or-expression->or parse)
       (PARSEBNF (or-expression->left))
       (PARSEBNF (or-expression->right))]
      [(and-expression? parse)
       (and-expression->and parse)
       (PARSEBNF (and-expression->left))
       (PARSEBNF (and-expression->right))])))