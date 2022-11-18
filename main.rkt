#lang scheme
(define ns (make-base-namespace))





;; Retorna una lista con numeros entre i y n (sin incluir) que no esten en la
;; lista dada.
;;
;; lista: Lista con los numeros que se excluyen del resultado
;; n: Limite superior del resultado
;; i: Limite inferior del resultado
(define (inverso_aux lista n i)
  (cond
    ((>= i n) 
      '())
    ((and (not (null? lista))
          (= i (car lista)))
      (inverso_aux (cdr lista) n (+ i 1)))
    (else 
      (cons i (inverso_aux lista n (+ i 1))))))

(define (inverso lista n)
  (inverso_aux lista n 0))










(define (umbral_simple lista umbral tipo)
  (if (null? lista) 
    '()
    (let ((res   (map  (lambda (x) (+ x 1))  (umbral_simple (cdr lista) umbral tipo)))
          (fn    (if (eq? tipo #\M) > <)));; >w<
      (if (fn (car lista) umbral)
          (cons 0 res)
          res))))



;; Devuelve una lista con los indices de los elementos de la lista dada que cumplen
;; la condicion de ser mayor o menor que el umbral.
;;
;; lista: Lista de numeros
;; umbral: Numero que se usa para comparar
;; tipo: Si es 'M' se usa >, si es 'm' se usa <
;; index: indice en la lista original del head de la lista dada
;; acc: Lista donde se acumulan los resultados
(define (umbral_cola_aux lista umbral tipo index acc)
  (if (null? lista)
      acc
      (let* ((fn    (if (eq? tipo #\M) > <))
             (acc2  (if (fn (car lista) umbral)
                        (append acc (list index))
                        acc)))
        (umbral_cola_aux (cdr lista) umbral tipo (+ index 1) acc2))))

(define (umbral_cola lista umbral tipo)
  (umbral_cola_aux lista umbral tipo 0 '()))










;; Devuelve la lista dada, con los elementos indicados en seleccion modificados por la funcion f.
;;
;; lista: Lista con los elementos a modificar
;; seleccion: Lista con los indices de los elementos a modificar (debe estar ordenada)
;; f: Funcion que se usa para modificar los elementos
(define (modsel_simple_aux lista seleccion f)
  (cond
    ((null? lista) 
      '())
    ((null? seleccion) 
      lista)
    ((eq? (car seleccion) 0)
      (cons (f (car lista)) 
            (modsel_simple_aux 
              (cdr lista) 
              (map (lambda (x) (- x 1)) (cdr seleccion)) 
              f)))
    (else
      (cons (   car lista ) 
            (modsel_simple_aux 
              (cdr lista) 
              (map (lambda (x) (- x 1))      seleccion ) 
              f)))))

(define (modsel_simple lista seleccion f)
  (modsel_simple_aux 
    lista 
    (sort seleccion (lambda (x y) (<= x y))) 
    f))



;; Devuelve la lista dada, con los elementos indicados en seleccion modificados por la funcion f.
;;
;; lista: Lista con los elementos a modificar
;; seleccion: Lista con los indices de los elementos a modificar (debe estar ordenada)
;; f: Funcion que se usa para modificar los elementos
;; i: Indice en la lista original del head de la lista dada
;; acc: Lista donde se guarda el resultado
(define (modsel_cola_aux lista seleccion f i acc)
  (cond
    ((null? lista)
      acc)
    ((null? seleccion)
      (append acc lista))
    ((= (car seleccion) i)
      (modsel_cola_aux (cdr lista) (cdr seleccion) f (+ i 1) (append acc (list (f (car lista))))))
    (else
      (modsel_cola_aux (cdr lista)      seleccion  f (+ i 1) (append acc (list    (car lista) ))))))

(define (modsel_cola lista seleccion f)
  (modsel_cola_aux 
    lista 
    (sort seleccion (lambda (x y) (<= x y))) 
    f 
    0 
    '()))










;; Devuelve la cantidad de numeros de la lista que son menores o mayores que el umbral,
;; y que al aplicarles la funcion f, el resultado sigue siendo menor o mayor segun el tipo dado.
;;
;; lista: Lista de numeros
;; umbral: Numero que se usa para comparar
;; fn: Funcion que se usa para modificar los elementos
;; tipo: Si es 'M' se usa >, si es 'm' se usa <
(define (estables_aux lista umbral fn tipo)
  (length (umbral_cola ;; Obtiene la cantidad de elementos de la lista nueva que cumplen la condicion.
            (map fn ;; Aplicar la funcion fn a cada elemento de la lista filtrada.
                 (map (lambda (x) (list-ref lista x)) ;; Convierte la lista de indices en una lista con sus elementos.
                      (umbral_cola lista umbral tipo))) ;; Todos los elementos que cumplen la condicion.
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
      (estables    (list-ref lista pos) (first params) (eval (second params) ns) (eval (third params) ns)))))


