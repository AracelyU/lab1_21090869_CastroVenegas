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
(define pixbit_3 (pixbit-d 0 2 0 30))
(define pixbit_4 (pixbit-d 1 0 1 40))
(define pixbit_5 (pixbit-d 1 1 0 50))
(define pixbit_6 (pixbit-d 1 2 0 60))
(define pixbit_7 (pixbit-d 2 0 0 70))
(define pixbit_8 (pixbit-d 2 1 0 80))
(define pixbit_9 (pixbit-d 2 2 0 90))

; definir una image 2
(define image_2 (image 3 3 pixbit_1 pixbit_2 pixbit_3 pixbit_4 pixbit_5 pixbit_6 pixbit_7 pixbit_8 pixbit_9))

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

; definir funciones utilizadas para el flipH y flipV

; Descripción: funcion que te entrega la posicion final en x
; Dom: image
; Rec: entero
(define largo_pos_x (lambda (image) (- (width_image image) 1)))

; Descripción: funcion que te entrega la posicion final en y
; Dom: image
; Rec: entero
(define largo_pos_y (lambda (image) (- (height_image image) 1)))


; Descripción: funcion que modifica la posicion en y segun pos_y
; Dom: posicion (int) x pixel [pixrgb-d / pixhex-d / pixbit-d]
; Rec: pixel [pixrgb-d / pixhex-d / pixbit-d]

(define modificar_posicion_pixel (lambda (pos_x pixel)
     (cond
       [(pixbit-d? pixel) (cambiar_y_bit pixel pos_x)]
       [(pixhex-d? pixel) (cambiar_y_hex pixel pos_x)]
       [(pixrgb-d? pixel) (cambiar_y_rgb pixel pos_x)]
       
       )))




; Descripción: funcion que te entrega una lista de las posiciones invertidas pixeles
; Dom: fila (lista) x image
; Rec: pixeles (lista)
(define flipH-formato (lambda (formato_fila image)

     (define flipHN (lambda (formato_fila pos_x)

         (cond
         [(null? formato_fila) null]
         [(< pos_x 0) null]
         [else (reverse (cons (modificar_posicion_pixel pos_x (car formato_fila)) (flipHN (cdr formato_fila) (- pos_x 1))))
               ])))

      (flipHN formato_fila (largo_pos_x image))))

; Descipción: funcion que entrega una fila de la matriz según n
; Dom: n (int) x formato_pixeles (lista)
; Rec: fila (lista)
(define fila_n (lambda (n lista)
    (if (null? lista)
        null
        (if (= (car(car lista)) n)
            (cons (car lista) (fila_n n (cdr lista)))
            (fila_n n (cdr lista))
            ))))

; Descripción: función que intercambia las posiciones del formato de pixeles de la imagen HORIZONTALMENTE
; Dom: image
; Rec: formato de pixeles de la image
(define flipH-cambio (lambda (image)

    (define flipHC (lambda (pos_y)   
        (if (> pos_y (largo_pos_y image))
             null
             (append (ordenar (flipH-formato (fila_n pos_y (format_image image)) image) image) (flipHC (+ pos_y 1)))
                 )
            ))
         
    (flipHC 0)
                ))

; Descripción: flipH
; Dom: image
; Rec: image
(define flipH (lambda (image_ingresado)
     (arreglar_image (image  (width_image image_ingresado) (height_image image_ingresado) (flipH-cambio image_ingresado)))

                ))

;-------------------------------------------- OTRAS FUNCIONES----------------------------------------------

; funcion para entregar un pixel en funcion de y
(define obtener_pixel_y (lambda (lista pos_y)
    (if (null? lista)
        null
        (if (= pos_y (cadr(car lista)))
            (car lista)
            (obtener_pixel_y (cdr lista) pos_y)
            ))))

; funcion que ordena una lista en funcion de y
(define ordenar_lista_y (lambda (lista pos_y image)
    (if (> pos_y (largo_pos_y image))
        null
        (if (< (largo_pos_y image) 2)
            lista
            (cons (obtener_pixel_y lista pos_y) (ordenar_lista_y lista (+ pos_y 1) image))
        ))))

; funcion que ordena el formato de la imagen por y
(define ordenar (lambda (lista image)
     (ordenar_lista_y lista 0 image)
                  ))


; Dom: imagen
; Rec: image
; Descripción: función que arregla el formato de la imagen
(define arreglar_image (lambda (image_ingresado)
        (if (= (length (format_image image_ingresado)) 1)
            (list (width_image image_ingresado) (height_image image_ingresado) (car (format_image image_ingresado)))
            image_ingresado
        )))

(define lista (format_image image_2))

(define lista_n (fila_n 0 lista))

(define lista_2n (flipH-formato lista_n image_2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define filtro_nulos (lambda (lista)
    (if (null? lista)
        null
        (if (null? (car lista))
            null
            (cons (car lista) (filtro_nulos (cdr lista)))))
                       ))






