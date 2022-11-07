#lang scheme


(define (inverso_aux lista n i)
  (if (< i n)
    (if (null? lista)
      (cons i (inverso_aux lista n (+ i 1)))
      (if (= i (car lista))
        (inverso_aux (cdr lista) n (+ i 1))
        (cons i (inverso_aux lista n (+ i 1)))))
    '()
  )
)

(define (inverso lista n)
  (inverso_aux lista n 0)
)


(define (umbral_simple lista umbral tipo)
  (if (null? lista) 
    '()
    (if (eq? tipo #\M)
      (if (> (car lista) umbral)
        (cons 0 (map (lambda (x) (+ x 1)) (umbral_simple (cdr lista) umbral tipo)))
        (map (lambda (x) (+ x 1)) (umbral_simple (cdr lista) umbral tipo))
      )
      (if (< (car lista) umbral)
        (cons 0 (map (lambda (x) (+ x 1)) (umbral_simple (cdr lista) umbral tipo)))
        (map (lambda (x) (+ x 1)) (umbral_simple (cdr lista) umbral tipo))
      )
    )
  )
)

(define (concatenar X Y)
  (if (null? X)
    Y
    (cons (car X) (concatenar (cdr X) Y))
  )
)
;; funcion anterior con recursion de cola
(define (umbral_cola_aux lista umbral tipo index acc)
  (if (null? lista)
    acc
    (if (eq? tipo #\M)
      (if (> (car lista) umbral)
        (umbral_cola_aux (cdr lista) umbral tipo (+ index 1) (concatenar acc (list index)))
        (umbral_cola_aux (cdr lista) umbral tipo (+ index 1) acc)
      )
      (if (< (car lista) umbral)
        (umbral_cola_aux (cdr lista) umbral tipo (+ index 1) (concatenar acc (list index)))
        (umbral_cola_aux (cdr lista) umbral tipo (+ index 1) acc)
      )
    )
  )
)

(define (umbral_cola lista umbral tipo)
  (umbral_cola_aux lista umbral tipo 0 '())
)


(define (modsel_simple_aux lista seleccion f)
  (if (null? lista)
    '()
    (if (null? seleccion)
      lista
      (if (eq? (car seleccion) 0)
        (cons (f (car lista)) (modsel_simple_aux (cdr lista) (map (lambda (x) (- x 1)) (cdr seleccion)) f))
        (cons (   car lista ) (modsel_simple_aux (cdr lista) (map (lambda (x) (- x 1))      seleccion ) f))
      )
    )
  )
)
(define (modsel_simple lista seleccion f)
  (modsel_simple_aux lista (sort seleccion (lambda (x y) (<= x y))) f)
)


;;; (define (modsel_cola lista seleccion f)
;;; ; Implementacion de la funcion
;;; )

;;; (define (estables lista umbral fM fm)
;;; ; Implementacion de la funcion
;;; )

;;; (define (query lista pos op params)
;;; ; Implementacion de la funcion
;;; )
