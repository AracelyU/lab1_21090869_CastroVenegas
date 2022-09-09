#lang racket

; Tda externos utilizados
(require "TDA_Pixrgb-d.rkt")
(require "TDA_Pixbit-d.rkt")
(require "TDA_Pixhex-d.rkt")

;-----------------------------------TDA IMAGEN-----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; El TDA representa una imagen del tipo rgb con diferentes formatos (bit, pixel, hexa).
; Se guarda una imagen como una lista de tres parametros
; (Width (int) X Height (int) X [pixbit-d* | pixrgb-d* | pixhex-d*]) que representan el ancho, largo
;  y el formato de la imagen respectivamente

;-----------------------------------CONSTRUCTOR -------------------------------------------------------------

; Dominio: entero X entero X [pixbit-d* | pixrgb-d* | pixhex-d*]
; Rec: Una representacion "image"
; Descripción: Función constructora de imágenes con bitmaps
; o pixmaps que incluye información de profundidad en cada pixel.
(define image (lambda (ancho largo . formato)
     (list ancho largo formato)))

;---------------------------------- SELECTORES--------------------------------------------------------------


; funciones para recorrer los 3 elementos de la entrada de la image
(define width_image (lambda (L) (car L))) ; primera posición
(define height_image (lambda (L) (cadr L))) ; segunda posición
(define format_image (lambda (L) (caddr L))) ; tercera posición (lista)



;------------------------------------------ IMAGENES ---------------------------------------------------------
; definiendo imagenes para probar las funcionalidades de la image

; definir 4 pixeles de un pixrgb-d
(define pixrgb_1 (pixrgb-d 0 0 10 10 10 10)) ; lista_1
(define pixrgb_2 (pixrgb-d 0 1 20 20 20 20))
(define pixrgb_3 (pixrgb-d 1 0 30 30 30 30))
(define pixrgb_4 (pixrgb-d 1 1 40 40 40 40))

; definir una image 1
(define image_1 (image 2 2 pixrgb_1 pixrgb_2 pixrgb_3 pixrgb_4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; definir 4 pixeles de un pixbit-d
(define pixbit_1 (pixbit-d 0 0 0 10)) ; lista_2
(define pixbit_2 (pixbit-d 0 1 1 20))
(define pixbit_3 (pixbit-d 1 0 0 30))
(define pixbit_4 (pixbit-d 1 1 1 4))

; definir una image 2
(define image_2 (image 2 2 pixbit_1 pixbit_2 pixbit_3 pixbit_4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
; definir 4 pixeles de un pixhex-d
(define pixhex_1 (pixhex-d 0 0 "#FF0000" 10)) ;lista_3
(define pixhex_2 (pixhex-d 0 1 "#0000FF" 20))
(define pixhex_3 (pixhex-d 1 0 "#00FF00" 30))
(define pixhex_4 (pixhex-d 1 1 "#FFFFFF" 40))

; definir una image 3
(define image_3 (image 2 2 pixhex_1 pixhex_2 pixhex_3 pixhex_4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SELECTORES ARREGLAR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,


; funciones selectores para recorrer las 6 posiciones de un pixel
;
(define pos_0 (lambda (L) (list-ref L 0))) ; x
(define pos_1 (lambda (L) (list-ref L 1))) ; y
(define pos_2 (lambda (L) (list-ref L 2))) ; bit, hex, c1
(define pos_3 (lambda (L) (list-ref L 3))) ; c2, bit
(define pos_4 (lambda (L) (list-ref L 4))) ; c3, d
(define pos_5 (lambda (L) (list-ref L 5))) ; profundidad d


;----------------------------- PERTENENCIA --------------------------------------------------------------------


; definir pixmap?
; Dom: image
; Rec: boleano
; Descripción: función que permite determinar si la imagen corresponde a un pixmap-d.
(define pixmap? (lambda (image)
        (andmap pixrgb-d? (format_image image))))


; definir bitmap?
; Dom: image
; Rec: boleano
; Descripción: función que permite determinar si la imagen corresponde a un bitmap-d.
(define bitmap? (lambda (image)
         (andmap pixbit-d? (format_image image))))


; definir hexmap?
; Dom: Image
; Rec: Boleano
; Descripción: unción que permite determinar si la imagen corresponde a un hexmap-d.
(define hexmap? (lambda (image)
          (andmap pixhex-d? (format_image image))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; fliH
(define largo_pos_x (lambda (image) (- (width_image image) 1)))
(define largo_pos_y (lambda (image) (- (height_image image) 1)))

(define modificar_posicion_pixel (lambda (pos_x pixel)
     (cond
       [(pixbit-d? pixel) (pixbit-d (pos_0 pixel) pos_x (pos_2 pixel) (pos_3 pixel))]
       [(pixbit-d? pixel) (pixhex-d (pos_0 pixel) pos_x (pos_2 pixel) (pos_3 pixel))]
       [(pixbit-d? pixel) (pixrgb-d (pos_0 pixel) pos_x (pos_2 pixel) (pos_3 pixel) (pos_4 pixel) (pos_5 pixel))]
       
       )))


(define pos_x_final? (lambda (pos_x pos_y image)
    (if (< pos_y (largo_pos_y image))
        (largo_pos_x image)
        pos_x
        )

                       ))

; funcion que te entrega una lista de las posiciones invertidas pixeles
(define flipH-formato (lambda (formato_fila image)

     (define flipHN (lambda (formato_fila pos_x)

         (cond
         [(null? formato_fila) null]
         [(< pos_x 0) null]
         [else (reverse (cons (modificar_posicion_pixel pos_x (car formato_fila)) (flipHN (cdr formato_fila) (- pos_x 1))))
               ]) 

          ))

      (flipHN formato_fila (largo_pos_x image))
                ))

(define filtro_nulos (lambda (lista)
    (if (null? lista)
        null
        (if (null? (car lista))
            null
            (cons (car lista) (filtro_nulos (cdr lista)))))
                       ))

(define fila_n (lambda (n lista)
    (if (null? lista)
        null
        (if (= (car(car lista)) n)
            (cons (car lista) (fila_n n (cdr lista)))
            (fila_n n (cdr lista))
            ))))

(define flipH (lambda (image)

    (define flipHC (lambda (pos_y)
         (if (>= (height_image image) 2)      
             (if (= pos_y (largo_pos_y image))
                 null
                 (append (flipH-formato (fila_n pos_y (format_image image)) image) (flipHC (+ pos_y 1)))
                 )
             (flipH-formato (fila_n pos_y (format_image image)) image)
            )))
         
    (flipHC 0)
                ))

(define lista (format_image image_1))

(define lista_n (fila_n 0 lista))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








