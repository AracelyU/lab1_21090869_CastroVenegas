#lang racket


#|
-----------------------------------TDA PIXBIT-D -----------------------------------------------------------------

----------------------------------REPRESENTACION-------------------------------------------------------------

 El TDA representa pixel tipo bitmap como una lista de 4 parametros, uno que contiene la coordenada x, otro
 para la coordenada y, uno que contenga el valor del bit y el último para la profundidad
 (int, int, bit([0, 1]), int)

|#
;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Dominio: x (int) X y (int) X bit ([0, 1]) X d (int)
; Recorrido: pixbit-d
; Descripción: Función constructora de un pixbit-d
(define pixbit-d (lambda (x y bit d) (list x y bit d)))

;----------------------------------- SELECTORES--------------------------------------------------------------

; Dominio: pixbit-d
; Recorrido: posición x (int)
; Descripción: Función para recuperar la posición x de un pixbit-d
(define x_bit (lambda (pixbit-d) (car pixbit-d)))

; Dominio: pixbit-d
; Recorrido: posición y (int)
; Descripción: Función para recuperar la posicion y de un pixbit-d
(define y_bit (lambda (pixbit-d) (cadr pixbit-d)))

; Dominio: pixbit-d
; Recorrido: bit (int)
; Descripción: Función para recuperar el bit de un pixrgb-d
(define bit (lambda (pixbit-d) (caddr pixbit-d)))

; Dominio: pixbit-d
; Recorrido: profundidad (int)
; Descripción: Función para recuperar la profundidad de un pixbit-d
(define d_bit (lambda (pixbit-d) (cadddr pixbit-d)))

; Verificar si es selector
; Dominio: image
; Recorrido: bit (int)
; Descripción: Función que recopila la cantidad de bit 0 del formato de pixeles
; Tipo de recursión: Cola
(define cantidad_bit (lambda (formato_pixeles bit_ingresado result)
    (if (null? formato_pixeles)
        result
        (if (= (bit (car formato_pixeles)) bit_ingresado)
            (cantidad_bit (cdr formato_pixeles) bit_ingresado (+ result 1))
            (cantidad_bit (cdr formato_pixeles) bit_ingresado result)))))

; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Dominio: x (int) X y (int) X bit (int) X d (int)
; Recorrido: boleano
; Descripción: Función que verifica si el argumento es un pixrgb-d
(define pixbit-d? (lambda (pixbit-d)
   (if (and (= (length pixbit-d) 4) (number? (x_bit pixbit-d)) (number? (y_bit pixbit-d)) (bit? (bit pixbit-d)) (number? (d_bit pixbit-d)))
       #t #f)))

; Dominio: bit (int)
; Recorrido: boleano
; Descripción: Función que verifica si el bit es un bit ([0, 1])
(define bit? (lambda (bit) (if (and (number? bit) (or (= bit 0) (= bit 1))) #t #f)))

; Dominio: pixbit-d
; Recorrido: boleano
; Descripción: Función que verifica si un bitmap fue comprimido
(define pixbit-d_compressed? (lambda (pixbit-d)
   (if (and (= (length pixbit-d) 4) (number? (x_bit pixbit-d)) (number? (y_bit pixbit-d)) (compressed_bit? (bit pixbit-d)) (number? (d_bit pixbit-d)))
       #t #f)))

; Dominio: pixbit-d
; Recorrido: boleano
; Descripción: Función que verifica si se comprimio un pixbit-d
(define compressed_bit? (lambda (bit) (if (and (number? bit) (= bit -1)) #t #f)))

; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Función que modifica la posicion x de un pixbit-d
(define cambiar_x_bit (lambda (pixbit-d_pasado x_nuevo)               
      (pixbit-d x_nuevo (y_bit pixbit-d_pasado) (bit pixbit-d_pasado) (d_bit pixbit-d_pasado))))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Función que modifica la posicion y de un pixbit-d
(define cambiar_y_bit (lambda (pixbit-d_pasado y_nuevo)               
      (pixbit-d (x_bit pixbit-d_pasado) y_nuevo (bit pixbit-d_pasado) (d_bit pixbit-d_pasado))))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Función que modifica el bit de un pixbit-d
(define cambiar_b_bit (lambda (pixbit-d_pasado b_nuevo)               
      (pixbit-d (x_bit pixbit-d_pasado) (y_bit pixbit-d_pasado) b_nuevo (d_bit pixbit-d_pasado))))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Función que modifica la profundidad de un pixbit-d
(define cambiar_d_bit (lambda (pixbit-d_pasado d_nuevo)               
      (pixbit-d (x_bit pixbit-d_pasado) (y_bit pixbit-d_pasado) (bit pixbit-d_pasado) d_nuevo)))

   
;-------------------------------------------- OTROS --------------------------------------------------

; Dominio: formato de pixeles (list)
; Recorrido: list
; Descripción: Función histograma que recopila numero de bit
(define histogram_bit (lambda (formato_image)
    (list (list (cantidad_bit formato_image 0 0) 0) (list (cantidad_bit formato_image 1 0) 1))))

; Dominio: histogram de bit
; Rec: int
; Descripción: función que obtiene el bit más repetido del histograma
(define bit_mayor (lambda (lista_bit)
    (if (> (car (car lista_bit)) (car (car (cdr lista_bit)))) 0 1)))

; Dominio: histogram de bit
; Recorrido: int
; Descripción: Función que obtiene el bit menos repetido del histograma
(define bit_menor (lambda (lista_bit)
    (if (< (car (car lista_bit)) (car (car (cdr lista_bit)))) 0 1)))

; Dominio: formato de pixeles (list) X elemento (int)
; Recorrido: formato de pixeles (list)
; Descripción: Función que crea una lista sin el bit más repetido
; Tipo de recursión: Natural
(define compress-formato-bit (lambda (lista elemento)
    (if (null? lista)
        null
        (if (= elemento (bit (car lista)))
            (cons (cambiar_b_bit (car lista) -1) (compress-formato-bit (cdr lista) elemento))
            (cons (car lista) (compress-formato-bit (cdr lista) elemento))))))

; Dominio: formato de pixeles (list) X elemento (int)
; Recorrido: formato de pixeles (list)
; Descripción: función que devuelve los valores perdidos tras compress
; Tipo de recursión: Natural
(define descompress-formato-bit (lambda (lista elemento)
    (if (null? lista)
        null
        (if (= (bit (car lista)) -1)
            (cons (cambiar_b_bit (car lista) elemento) (descompress-formato-bit (cdr lista) elemento))
            (cons (car lista) (descompress-formato-bit (cdr lista) elemento))))))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Función que invierte el color de bit de un pixbit-d
(define invertColorBit (lambda (pixbit-d_pasado)
    (if (pixbit-d? pixbit-d_pasado)
        (if (= (bit pixbit-d_pasado) 0)
            (cambiar_b_bit pixbit-d_pasado 1)
            (cambiar_b_bit pixbit-d_pasado 0))
        pixbit-d_pasado)))

; Dominio: formato de pixeles (list) X largo (int)
; Recorrido: string
; Descripción: Función que convierte el formato de pixeles en una cadena de string, pixbit->string
(define pixbit->string (lambda (formato_image largo)
                            
    (define fila_bit (lambda (formato_image fila)
        (if (null? formato_image)
            "\n"
            (if (= (x_bit (car formato_image)) fila)
                (string-append (number->string (bit (car formato_image))) " " (fila_bit (cdr formato_image) fila))
                (fila_bit (cdr formato_image) fila)))))

    (define formar_string (lambda (formato_image largo fila)
          (if (<= fila largo)
              (string-append (fila_bit formato_image fila) (formar_string formato_image largo (+ fila 1)))
               "\n")))
                         
    (formar_string formato_image largo 0)))


; Dominio: formato de pixeles (list)
; Recorrido: list
; Descripción: Función que obtiene pixeles con distintas profundidades
; Tipo de recursión: Natural

(define profundidad_lista (lambda (formato_pixeles)
                            
     (define igual_profundidad (lambda (formato_pixeles e)
             (if (null? formato_pixeles)
                 null
                 (if (= (d_bit (car formato_pixeles)) e)
                     (cons (car formato_pixeles) (igual_profundidad (cdr formato_pixeles) e))
                     (igual_profundidad (cdr formato_pixeles) e)))))


     (define filtro_profundidad (lambda (formato_pixeles e)
         (if (null? formato_pixeles)
              null
              (if (= (d_bit (car formato_pixeles)) e)
                  (filtro_profundidad (cdr formato_pixeles) e)
                  (cons (car formato_pixeles) (filtro_profundidad (cdr formato_pixeles) e))))))

     (if (null? formato_pixeles)
          null
         (cons (igual_profundidad formato_pixeles (d_bit (car formato_pixeles)))
               (profundidad_lista (filtro_profundidad formato_pixeles (d_bit (car formato_pixeles))))))))


; Dominio: formato de pixeles (list)
; Recorrido: list
; Descripción: Función que recopila la cantidad de elemento de cada tipo de una lista



; exportar la funcion al exterior
(provide (all-defined-out))