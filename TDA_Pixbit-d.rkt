#lang racket

; exportar la funcion al exterior
(provide (all-defined-out))


;-----------------------------------TDA PIXBIT-D -----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; El TDA representa pixel tipo rgb con parametros (posicion_x, posicion_y, binario [0/1], profundidad).
; Se representa un pixel como una lista de custro parametros
; (posicionX, posicionY, Binario, profundidad)

;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Descripción: formato de un pixbit-d
; Dom: x int X y int X bit int X d int
; Rec: pixbit-d
(define pixbit-d (lambda (x y bit d)
               (list x y bit d)))


;----------------------------------- SELECTORES--------------------------------------------------------------

; Descripción: obtener la posición x de un pixbit-d
; Dom: pixbit-d
; Rec: posición x (int)
(define x_bit (lambda (pixbit-d) (car pixbit-d)))

; Descripción: obtener la posicion y de un pixbit-d
; Dom: pixbit-d
; Rec: posición y (int)
(define y_bit (lambda (pixbit-d) (cadr pixbit-d)))

; Descripción: obtener el bit de un pixrgb-d
; Dom: pixbit-d
; Rec: bit (int)
(define bit (lambda (pixbit-d) (caddr pixbit-d)))

; Descripción: obtener la profundidad de un pixbit-d
; Dom: pixbit-d
; Rec: d (int)
(define d_bit (lambda (pixbit-d) (cadddr pixbit-d)))

; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Descripción: función que verifica si el argumento es un pixrgb-d
; Dom: x (int) X y (int) X bit (int) X d (int)
; Rec: Boleano
(define pixbit-d? (lambda (pixbit-d)
   (if (and (= (length pixbit-d) 4)
            (number? (x_bit pixbit-d))
            (number? (y_bit pixbit-d))
            (number? (bit pixbit-d)) (bit? (bit pixbit-d))
            (number? (d_bit pixbit-d))
       
            ) #t #f)))

; Descripción: función que verifica si el bit es un bit
; Dom: int
; Rec: Boleano
(define bit? (lambda (bit)
     (if (or (= bit 0) (= bit 1)) #t #f)
                   ))

; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Descripción: modificar la posicion x de un pixbit-d
; Dom: pixbit-d
; Rec: pixbit-d
(define cambiar_x_bit (lambda (pixbit-d_pasado x_nuevo)               
      (pixbit-d x_nuevo (y_bit pixbit-d_pasado) (bit pixbit-d_pasado) (d_bit pixbit-d_pasado))))


; Descripción: modificar la posicion y de un pixbit-d
; Dom: pixbit-d
; Rec: pixbit-d
(define cambiar_y_bit (lambda (pixbit-d_pasado y_nuevo)               
      (pixbit-d (x_bit pixbit-d_pasado) y_nuevo (bit pixbit-d_pasado) (d_bit pixbit-d_pasado))))


; Descripción: modificar el bit de un pixbit-d
; Dom: pixbit-d
; Rec: pixbit-d
(define cambiar_b_bit (lambda (pixbit-d_pasado b_nuevo)               
      (pixbit-d (x_bit pixbit-d_pasado) (y_bit pixbit-d_pasado) b_nuevo (d_bit pixbit-d_pasado))))


; Descripción: modificar la profundidad de un pixbit-d
; Dom: pixbit-d
; Rec: pixbit-d
(define cambiar_d_bit (lambda (pixbit-d_pasado d_nuevo)               
      (pixbit-d (x_bit pixbit-d_pasado) (y_bit pixbit-d_pasado) (bit pixbit-d_pasado) d_nuevo)))

;------------------------------------------- SELECTORES----------------------------------------------

; Descripción: función que recopila número de bit 0
; Dom: image
; Rec: entero
; tipo de recursión: cola
(define cantidad_bit (lambda (formato_image bit_ingresado result)
    (if (null? formato_image)
        result
        (if (= (bit (car formato_image)) bit_ingresado)
            (cantidad_bit (cdr formato_image) bit_ingresado (+ result 1))
            (cantidad_bit (cdr formato_image) bit_ingresado result)
            
            ))))

;-------------------------------------------- OTROS --------------------------------------------------

; Descripción: función histograma que recopila numero de bit
; Dom: lista (pixeles)
; Rec: lista
(define histograma_bit (lambda (formato_image)
    (list (list (cantidad_bit formato_image 0 0) 0) (list (cantidad_bit formato_image 1 0) 1))))


; Descripción: función que obtiene el bit más repetido del histograma
; Dom: lista de histograma
; Rec: entero
(define bit_mayor (lambda (lista_bit)
    (if (> (car (car lista_bit)) (car (car (cdr lista_bit))))
        0
        1
        )))

; Descripción: función que crea una lista sin el bit más repetido
(define compress-formato-bit (lambda (lista elemento)
    (if (null? lista)
        null
        (if (= elemento (bit (car lista)))
            (compress-formato-bit (cdr lista) elemento)
            (cons (car lista) (compress-formato-bit (cdr lista) elemento))))))



; Descripción: Invertir color de bit de un pixbit-d
; Dom: pixbit-d
; Rec: pixbit-d
(define invertColorBit (lambda (pixbit-d_pasado)
    (if (= (bit pixbit-d_pasado) 0)
        (cambiar_b_bit pixbit-d_pasado 1)
        (cambiar_b_bit pixbit-d_pasado 0)
                         )))


(define pix (pixbit-d 0 0 0 12))