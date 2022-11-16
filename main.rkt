#lang scheme
(define ns (make-base-namespace))



;; TODO: representar funciones como funciones matematicas para mayor legibilidad.
;; TODO: usar cond en vez de if anidados.
;; TODO: usar let* para evitar let anidados.


(define (inverso_aux lista n i)
  (cond
    ((>= i n) 
      '())
    ((and (not (null? lista))
          (= i (car lista)))
      (inverso_aux (cdr lista) n (+ i 1)))
    (else 
      (cons i (inverso_aux lista n (+ i 1))))))

;; Recibe una lista de numeros y un numero, y retorna una lista con todos los
;; numeros entre 0 y n ([0..N[) que no esten en la lista.
;;
;; [0..N[ - L
;;
;; lista: Lista de numeros.
;; n: Numero que indica el superior.
(define (inverso lista n)
  (inverso_aux lista n 0))












;; Recibe una lista de numeros (lista), un numero (umbral) y un caracter (tipo).
;; Si el tipo es 'M' retorna una lista con todas las posiciones de los elementos lista que sean mayores que umbral.
;; Si es 'm' retorna las posiciones de los que son menores.
;;
;; Esta funcion lo implementa usando recursividad simple.
(define (umbral_simple lista umbral tipo)
  (if (null? lista) 
    '()
    (let ((res   (map  (lambda (x) (+ x 1))  (umbral_simple (cdr lista) umbral tipo)))
          (fn    (if (eq? tipo #\M) > <)));; >w<
      (if (fn (car lista) umbral)
          (cons 0 res)
          res))))




(define (umbral_cola_aux lista umbral tipo index acc)
  (if (null? lista)
      acc
      (let* ((fn    (if (eq? tipo #\M) > <))
             (acc2  (if (fn (car lista) umbral)
                        (append acc (list index))
                        acc)))
        (umbral_cola_aux (cdr lista) umbral tipo (+ index 1) acc2))))

;; Recibe una lista de numeros (lista), un numero (umbral) y un caracter (tipo).
;; Si el tipo es 'M' retorna una lista con todas las posiciones de los elementos lista que sean mayores que umbral.
;; Si es 'm' retorna las posiciones de los que son menores.
;;
;; Esta funcion lo implementa usando recursion de cola.
(define (umbral_cola lista umbral tipo)
  (umbral_cola_aux lista umbral tipo 0 '()))













(define (modsel_simple_aux lista seleccion f)
  (cond
    ((null? lista) 
      '())
    ((null? seleccion) 
      lista)
    ((eq? (car seleccion) 0)
      (cons (f (car lista)) (modsel_simple_aux (cdr lista) (map (lambda (x) (- x 1)) (cdr seleccion)) f)))
    (else
      (cons (   car lista ) (modsel_simple_aux (cdr lista) (map (lambda (x) (- x 1))      seleccion ) f)))))


;; Recibe dos listas de numeros (lista y seleccion) y una funcion lambda (f).
;; Por cada numero en la lista, si su indice esta en seleccion entonces se le
;; debe aplicar la funcion f, en caso contrario el numero se matiene igual.
;;
;;Esta funcion lo implementa usando recursion simple.
(define (modsel_simple lista seleccion f)
  (modsel_simple_aux 
    lista 
    (sort seleccion (lambda (x y) (<= x y))) 
    f))



(define (modsel_cola_aux lista seleccion f i acc)
  (if (null? lista)
      acc
      (if (null? seleccion)
          (append acc lista)
          (if (= (car seleccion) i)
              (modsel_cola_aux (cdr lista) (cdr seleccion) f (+ i 1) (append acc (list (f (car lista)))))
              (modsel_cola_aux (cdr lista)      seleccion  f (+ i 1) (append acc (list    (car lista) )))))))
;; Recibe dos listas de numeros (lista y seleccion) y una funcion lambda (f).
;; Por cada numero en la lista, si su indice esta en seleccion entonces se le
;; debe aplicar la funcion f, en caso contrario el numero se matiene igual.
;;
;;Esta funcion lo implementa usando recursion de cola.
(define (modsel_cola lista seleccion f)
  (modsel_cola_aux 
    lista 
    (sort seleccion (lambda (x y) (<= x y))) 
    f 
    0 
    '()))



















(define (estables_aux lista umbral fn tipo)
  (length (umbral_cola 
            (map fn 
                (map (lambda (x) (list-ref lista x)) 
                      (umbral_cola lista umbral tipo))) 
            umbral 
            tipo)))

(define (estables lista umbral fM fm)
  (list (estables_aux lista umbral fM #\M)
        (estables_aux lista umbral fm #\m)))


















(define (query lista pos op params)
  (cond
    ((= op 1)
      (umbral_cola (list-ref lista pos) (first params) (second params)))
    ((= op 2)
      (modsel_cola (list-ref lista pos) (first params) (eval (second params) ns)))
    ((= op 3)
      (estables (list-ref lista pos) (first params) (eval (second params) ns) (eval (third params) ns)))))
















(display "\n")
;; 1. Inverso
(display ">(inverso '(1 3 7) 10)\n")
(display "(0 2 4 5 6 8 9)\n")
(display (inverso '(1 3 7) 10))
(display "\n")

(display "\n")
;; 2. Umbral
(display ">(umbral_simple '(15 2 1 3 27 5 10) 5 #\\M)\n")
(display "(0 4 6)\n")
(display (umbral_simple '(15 2 1 3 27 5 10) 5 #\M))
(display "\n")
(display ">(umbral_cola '(15 2 1 3 27 5 10) 5 #\\m)\n")
(display "(1 2 3)\n")
(display (umbral_cola '(15 2 1 3 27 5 10) 5 #\m))
(display "\n")

(display "\n")
;; 3. Modificar seleccionados
(display ">(modsel_simple '(15 2 1 3 27 5 10) '(0 4 6) (lambda (x) (modulo x 2)))\n")
(display "(1 2 1 3 1 5 0)\n")
(println (modsel_simple '(15 2 1 3 27 5 10) '(0 4 6) (lambda (x) (modulo x 2))))
(display ">(modsel_simple '(15 2 1 3 27 5 10) '(3 1 2) (lambda (x) (+ x 5)))\n")
(display "(15 7 6 8 27 5 10)\n")
(display (modsel_simple '(15 2 1 3 27 5 10) '(3 1 2) (lambda (x) (+ x 5))))
(display "\n")

(display "\n")
;; 4. Estables
(display ">(estables '(15 2 1 3 27 5 10) 5 (lambda (x) (/ x 2)) (lambda (x) (* x 2))\n")
(display "(2 1)\n")
(display (estables '(15 2 1 3 27 5 10) 5 (lambda (x) (/ x 2)) (lambda (x) (* x 2))))
(display "\n")

(display "\n")
;; 5. Query
(display ">(query '((0 1 2 3 4) (4 3 2 1 0) (15 2 1 3 27 5 10)) 1 1 '(1 #\\M))\n")
(display "(0 1 2)\n")
(display (query '((0 1 2 3 4) (4 3 2 1 0) (15 2 1 3 27 5 10)) 1 1 '(1 #\M)))
(display "\n")
(display ">(query '((0 1 2 3 4) (4 3 2 1 0) (15 2 1 3 27 5 10)) 0 2 '((0 4) (lambda (x) (+ x 100))))\n")
(display "(100 1 2 3 104)\n")
(display (query '((0 1 2 3 4) (4 3 2 1 0) (15 2 1 3 27 5 10)) 0 2 '((0 4) (lambda (x) (+ x 100)))))
(display "\n")
(display ">(query '((0 1 2 3 4) (4 3 2 1 0) (15 2 1 3 27 5 10)) 2 3 '(5 (lambda (x) (/ x 2)) (lambda (x) (* x 2))))\n")
(display "(2 1)\n")
(display (query '((0 1 2 3 4) (4 3 2 1 0) (15 2 1 3 27 5 10)) 2 3 '(5 (lambda (x) (/ x 2)) (lambda (x) (* x 2)))))
(display "\n")

