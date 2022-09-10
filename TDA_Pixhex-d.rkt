#lang racket

; exportar la funcion al exterior
(provide (all-defined-out))

;-----------------------------------TDA PIXHEX-D -----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; El TDA representa pixel tipo rgb con parametros (posicion_x, posicion_y, string, profundidad).
; Se representa un pixel como una lista de custro parametros
; (posicionX, posicionY, formato_hexa, profundidad)

;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Descripción: formato de un pixhex-d
; Dom: x int X y int X hexa int X d int
; Rec: pixhex-d
(define pixhex-d (lambda (x y hex d)
               (list x y hex d)))

;----------------------------------- SELECTORES--------------------------------------------------------------

; Descripción: obtener la posición x de un pixhex-d
; Dom: pixhex-d
; Rec: posición x (int)
(define x_hex (lambda (pixhex-d) (car pixhex-d)))

; Descripción: obtener la posicion y de un pixhex-d
; Dom: pixhex-d
; Rec: posición y (int)
(define y_hex (lambda (pixhex-d) (cadr pixhex-d)))

; Descripción: obtener el bit de un pixhex-d
; Dom: pixhex-d
; Rec: hexa (string)
(define hex (lambda (pixhex-d) (caddr pixhex-d)))

; Descripción: obtener la profundidad de un pixhex-d
; Dom: pixhex-d
; Rec: d (int)
(define d_hex (lambda (pixhex-d) (cadddr pixhex-d)))

; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Descripción: función que verifica si el argumento es un pixhex-d
; Dom: x (int) X y (int) X hex (string) X d (int)
; Rec: Boleano
(define pixhex-d? (lambda (pixhex-d)
    (if (and (= (length pixhex-d) 4)
             (number? (x_hex pixhex-d))
             (number? (y_hex pixhex-d))
             (string? (hex pixhex-d))
             (number? (d_hex pixhex-d))

             ) #t #f)))

; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Descripción: modificar la posicion x de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_x_hex (lambda (pixhex-d_pasado x_nuevo)               
      (pixhex-d x_nuevo (y_hex pixhex-d_pasado) (hex pixhex-d_pasado) (d_hex pixhex-d_pasado))))


; Descripción: modificar la posicion y de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_y_hex (lambda (pixhex-d_pasado y_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) y_nuevo (hex pixhex-d_pasado) (d_hex pixhex-d_pasado))))


; Descripción: modificar el string hex de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_h_hex (lambda (pixhex-d_pasado h_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) (y_hex pixhex-d_pasado) h_nuevo (d_hex pixhex-d_pasado))))


; Descripción: modificar la profundidad de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_d_hex (lambda (pixhex-d_pasado d_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) (y_hex pixhex-d_pasado) (hex pixhex-d_pasado) d_nuevo)))

(define lista_hex (pixhex-d 0 0 "FFFF" 10))


;--------------------------------------- SELECTORES ----------------------------------------------

; Descripción: función que filtra la lista por los elementos iguales a e
; Dom: lista x elemento
; Rec: lista
; tipo de recursión: Natural
(define filtro_iguales_hex (lambda (formato_image e)
    (if (null? formato_image)
        null
        (if (not (string-ci=? (hex (car formato_image)) e))
            (cons (car formato_image) (filtro_iguales_hex (cdr formato_image) e))
            (filtro_iguales_hex (cdr formato_image) e)))))

; Descripción: función que cuenta los elementos iguales a e en una lista
; Dom: lista (pixeles) x elemento
; Rec: lista
; tipo de recursión: cola
(define hex_iguales (lambda (formato_image e result)
       (if (null? formato_image)
           result
           (if (string-ci=? (hex (car formato_image)) e)
               (hex_iguales (cdr formato_image) e (+ result 1))
               (hex_iguales (cdr formato_image) e result)))))


;---------------------------------------------------- OTROS -----------------------------------------------------------------------------------
; definir 4 pixeles de un pixhex-d
(define pixhex_1 (pixhex-d 0 0 "#FF0000" 10)) ;lista_3
(define pixhex_2 (pixhex-d 0 1 "#0000FF" 20))
(define pixhex_3 (pixhex-d 1 0 "#0000FF" 30))
(define pixhex_4 (pixhex-d 1 1 "#FFFFFF" 40))


; definir una image 3
(define lista_3 (list pixhex_1 pixhex_2 pixhex_3 pixhex_4 pixhex_4 pixhex_4 pixhex_4))



; Descripción: función que recopila la cantidad de elemento de cada tipo de una lista
; Dom: lista (pixeles)
; Rec: lista
(define histograma_hex (lambda (formato_image)
    (if (null? formato_image)
        null
        (cons (list (hex_iguales formato_image (hex (car formato_image)) 0) (hex (car formato_image))) (histograma_hex (filtro_iguales_hex formato_image (hex (car formato_image))))))))


(define lista (histograma_hex lista_3))

; Descripción: función que obtiene el hex más repetido del histograma
; Dom: lista del histograma
; Rec: lista
(define hex_mayor (lambda (lista_hex result)
    (if (null? lista_hex)
        (car (cdr result))
        (if (> (car(car lista_hex)) (car result))
            (hex_mayor (cdr lista_hex) (car lista_hex))
            (hex_mayor (cdr lista_hex) result)))))















