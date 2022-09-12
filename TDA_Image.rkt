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
(define ancho_image (lambda (L) (car L))) ; primera posición
(define largo_image (lambda (L) (cadr L))) ; segunda posición
(define formato_image (lambda (L) (caddr L))) ; tercera posición (lista)

; Descripción: funcion que te entrega la posicion final en x
; Dom: image
; Rec: entero
(define largo_pos_x (lambda (image) (- (ancho_image image) 1)))

; Descripción: funcion que te entrega la posicion final en y
; Dom: image
; Rec: entero
(define largo_pos_y (lambda (image) (- (largo_image image) 1)))

; ------------------------------- MODIFICADORES------------------------------------------------------


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


;----------------------------- PERTENENCIA --------------------------------------------------------------------


; definir pixmap?
; Dom: image
; Rec: boleano
; Descripción: función que permite determinar si la imagen corresponde a un pixmap-d.
; Constructor
(define pixmap? (lambda (image)
        (andmap pixrgb-d? (formato_image image))))


; definir bitmap?
; Dom: image
; Rec: boleano
; Descripción: función que permite determinar si la imagen corresponde a un bitmap-d.
; Pertencencia
(define bitmap? (lambda (image)
         (andmap pixbit-d? (formato_image image))))


; definir hexmap?
; Dom: Image
; Rec: Boleano
; Descripción: función que permite determinar si la imagen corresponde a un hexmap-d.
; Pertenencia
(define hexmap? (lambda (image)
          (andmap pixhex-d? (formato_image image))))

; definir compressed?
; Dom: Image
; Rec: Boleano
; Descripción: función que permite determinar si la imagen sufrio una compresión o no
; Pertenencia
(define compressed? (lambda (image)
  (if (= (length (formato_image image)) (* (ancho_image image) (largo_image image)))
      #f
      #t)))

;----------------------------------------- SELECTORES ----------------------------------------------

; Descripción: funcion que entrega una fila de la matriz según n
; Dom: n (int) x formato_pixeles (lista)
; Rec: fila (lista)
; tipo de recursión: Natural
; Selector
(define fila_n (lambda (n lista)
    (if (null? lista)
        null
        (if (= (car(car lista)) n)
            (cons (car lista) (fila_n n (cdr lista)))
            (fila_n n (cdr lista))))))

; Descripción: funcion que encuentra un pixel según pos_x e pos_y
; Dom: lista
; Rec: pixel
(define encontrar_pixel (lambda (lista pos_x pos_y)
    (if (null? lista)
        null
        (if (and (= (car (car lista)) pos_x) (= (cadr (car lista)) pos_y))
            (car lista)
            (encontrar_pixel (cdr lista) pos_x pos_y)))))

;----------------------------------------- MODIFICADORES ---------------------------------------------


; Descripción: funcion que ordena formato de la imagen
; Dom: lista x int x image
; Rec: lista
; tipo de recursión: Natural
(define ordenar_formato (lambda (lista pos_x pos_y image contador)
      (if (= contador (* (ancho_image image) (largo_image image)))
          null
          (cond
            [(<= pos_y (largo_pos_y image)) (cons (encontrar_pixel lista pos_x pos_y) (ordenar_formato lista pos_x (+ pos_y 1) image (+ contador 1)))]
            [else (ordenar_formato lista (+ pos_x 1) 0 image contador)]))))


; Dom: imagen
; Rec: image
; Descripción: función que arregla el formato de la imagen
(define arreglar_image (lambda (image_ingresado)
        (if (= (length (formato_image image_ingresado)) 1)
            (list (ancho_image image_ingresado) (largo_image image_ingresado) (car (formato_image image_ingresado)))
            image_ingresado)))

; Descripción: funcion que modifica la posicion en y e x segun pos_y y pox_x
; Dom: posicion_y (int) x posicion_x (int) x pixel [pixrgb-d / pixhex-d / pixbit-d]
; Rec: pixel [pixrgb-d / pixhex-d / pixbit-d]
; Modificador
(define modificar_posicion_pixel (lambda (pos_y pos_x pixel)
     (cond
       [(pixbit-d? pixel) (cambiar_x_bit (cambiar_y_bit pixel pos_y) pos_x)]
       [(pixhex-d? pixel) (cambiar_x_hex (cambiar_y_hex pixel pos_y) pos_x)]
       [(pixrgb-d? pixel) (cambiar_x_rgb (cambiar_y_rgb pixel pos_y) pos_x)])))

; Descripción: funcion que te entrega una lista con las posiciones invertidas horizontalmente 
; Dom: fila (lista) x image
; Rec: pixeles (lista)
; tipo de recursión: Natural
; Modificador
(define flipH-formato (lambda (formato_fila pos_x image)
     (define flipHN (lambda (formato_fila pos_y pos_x)
         (cond
         [(null? formato_fila) null]
         [(< pos_y 0) null]
         [else (reverse (cons (modificar_posicion_pixel pos_y pos_x (car formato_fila)) (flipHN (cdr formato_fila) (- pos_y 1) pos_x)))])))

      (flipHN formato_fila (largo_pos_y image) pos_x)))

; Descripción: función flipH
; Dom: image
; Rec: image
; tipo de recursión: Natural
(define flipH (lambda (image_ingresado)
     (define flipHC (lambda (pos_y fila image)   
        (if (> pos_y (largo_pos_y image))
             null
             (append (flipH-formato (fila_n fila (formato_image image)) fila image) (flipHC (+ pos_y 1) (+ fila 1) image)))))
                
     (arreglar_image (image (ancho_image image_ingresado) (largo_image image_ingresado) (ordenar_formato (flipHC 0 0 image_ingresado) 0 0 image_ingresado 0)))))


; Descripción: funcion que te entrega una lista con las posiciones invertidas verticalmente 
; Dom: lista de pixeles x posicion n
; Rec: lista
; tipo de recursión: Natural
; Modificador
(define flipV-formato (lambda (fila_n pos_x pos_y)
    (if (null? fila_n)
        null
        (cons (modificar_posicion_pixel pos_y pos_x (car fila_n)) (flipV-formato (cdr fila_n) pos_x (+ pos_y 1))))))

; Descripción: flipV, apoyada por funciones donde algunas utilizan recursión natural 
; Dom: image
; Rec: image
; tipo de recursión: natural
(define flipV (lambda (image_ingresado)   
    (define flipVC (lambda (pos_x fila pos_y image)
        (if (< pos_x 0)
            null
            (append (flipV-formato (fila_n fila (formato_image image)) pos_x pos_y) (flipVC (- pos_x 1) (+ fila 1) pos_y image)))))
                
    (arreglar_image (image (ancho_image image_ingresado) (largo_image image_ingresado) (ordenar_formato (flipVC (largo_pos_x image_ingresado) 0 0 image_ingresado) 0 0 image_ingresado 0)))))


; Descripción: funcion que te entrega una lista de las posiciones invertidas pixeles
; Dom: fila (lista) x image
; Rec: pixeles (lista)
; tipo de recursión: Natural
; Modificador
(define rotate90-formato (lambda (formato_fila image pos_y)
     (define rotate (lambda (formato_fila pos_y pos_x)
         (cond
         [(null? formato_fila) null]
         [(> pos_x (largo_pos_x image)) null]
         [else (cons (modificar_posicion_pixel pos_y pos_x (car formato_fila)) (rotate (cdr formato_fila) pos_y (+ pos_x 1)))
               ])))
      (rotate formato_fila pos_y 0)))


; Descripción: rotate90, apoyada por funciones donde algunas utilizan recursión natural 
; Dom: image
; Rec: image
; tipo de recursión: Natural
(define rotate90 (lambda (image_ingresado)
    (define rotateC (lambda (pos_x fila pos_y image)
        (if (< pos_y 0)
            null
            (append (rotate90-formato (fila_n fila (formato_image image)) image pos_y) (rotateC pos_x (+ fila 1) (- pos_y 1) image))
                   )))
                   
   (arreglar_image (image (ancho_image image_ingresado) (largo_image image_ingresado) (ordenar_formato (rotateC 0 0 (largo_pos_y image_ingresado) image_ingresado) 0 0 image_ingresado 0)))))


;-------------------------------------------- OTRAS FUNCIONES----------------------------------------------

; Descripción: Histograma
; Dom: image
; Rec: lista
(define histograma (lambda (image)
    (cond
      [(pixmap? image) (histograma_rgb (formato_image image))]
      [(bitmap? image) (histograma_bit (formato_image image))]
      [(hexmap? image) (histograma_hex (formato_image image))]
      [else image])))


; Dom: Lista
; Rec: Lista
; función que cambia un pixel rgb a hexa
(define convertir_rgb_hex (lambda (pixel)

    ; función auxiliar para convertir un numero a hex
    (define valor_hex (lambda (a)
       (cond
          [(= a 1) "1"][(= a 2) "2"][(= a 3) "3"][(= a 4) "4"]
          [(= a 5) "5"][(= a 6) "6"][(= a 7) "7"][(= a 8) "8"]
          [(= a 9) "9"][(= a 10) "A"][(= a 11) "B"][(= a 12) "C"]
          [(= a 13) "D"][(= a 14) "E"][(= a 15) "F"][else "0"])))

    ; función auxiliar que transforma un c1 a hex
    (define rgb->hex (lambda (a)
       (string-append (valor_hex (quotient a 16)) (valor_hex (remainder a 16)))))                 

    ; función auxiliar que tranforma un color rgb->hex
    (define convertir_rgb (lambda (c1 c2 c3)
        (string-append (rgb->hex c1)(rgb->hex c2)(rgb->hex c3))))

   (cambiar_h_hex pixel (convertir_rgb (c1_rgb pixel) (c2_rgb pixel) (c3_rgb pixel)))))


; Dom: Image
; Rec: Image
; funcion que pasa de rgb a hexa, en caso de que no sea rgb retorna f
(define imgRGB->imgHex (lambda (image_rgb)
  (if (pixmap? image_rgb)
      (arreglar_image (image (ancho_image image_rgb) (largo_image image_rgb) (map convertir_rgb_hex (formato_image image_rgb))))    
      image_rgb))) ;si se ingresa una imagen distinta a rgb se retorna la imagen sin cambios


; Descripción: Compress
; Dom: image
; Rec: image
(define compress (lambda (image_ingresada)
              
    (cond
      [(bitmap? image_ingresada)
       (arreglar_image(image
                       (ancho_image image_ingresada)
                       (largo_image image_ingresada)
                       (compress-formato-bit (formato_image image_ingresada) (bit_mayor (histograma image_ingresada)))))]

      [(hexmap? image_ingresada)
       (arreglar_image (image
                        (ancho_image image_ingresada)
                        (largo_image image_ingresada)
                        (compress-formato-hex (formato_image image_ingresada) (hex_mayor (histograma image_ingresada) (car (histograma image_ingresada))))))]

      [(pixmap? image_ingresada)
       (arreglar_image (image
                        (ancho_image image_ingresada)
                        (largo_image image_ingresada)
                        (compress-formato-rgb (formato_image image_ingresada) (rgb_mayor (histograma image_ingresada) (car (histograma image_ingresada))))))])))

; Descripción: Descompress
; Dom: image
; Rec: image
(define descompress (lambda (image_ingresada)
    (cond
      [(bitmap? image_ingresada) (arreglar_image (image (ancho_image image_ingresada) (largo_image image_ingresada) (descompress-formato-bit (formato_image image_ingresada) (bit_mayor (histograma image_ingresada)))))]
      [(hexmap? image_ingresada) (arreglar_image (image (ancho_image image_ingresada) (largo_image image_ingresada) (descompress-formato-hex (formato_image image_ingresada) (hex_mayor (histograma image_ingresada) (car (histograma image_ingresada))))))]
      [else image_ingresada]
      )))



; Descripción: edit
; Dom: image
; Rec: image
(define edit (lambda (filtro image_ingresado)
     (define map_edit (lambda (filtro lista)
       (if (null? lista)
           null
           (cons (filtro (car lista)) (map_edit filtro (cdr lista))))))
               
       (arreglar_image (image (ancho_image image_ingresado) (largo_image image_ingresado) (map_edit filtro (formato_image image_ingresado))))))



; Descripción: image->string
; Dom: image x función
; Rec: string
(define image->string (lambda (image funcion_pixel)
        (funcion_pixel (formato_image image) (largo_pos_y image))))


;-----------------------------------------------------------------

(define filtro_nulos (lambda (lista)
    (if (null? lista)
        null
        (if (null? (car lista))
            null
            (cons (car lista) (filtro_nulos (cdr lista)))))
                       ))

(define lista_1 (formato_image image_1))
(define lista_2 (formato_image image_2))
(define lista_3 (formato_image image_3))


(define lista (formato_image image_1))
(define lista_n (fila_n 0 lista))
(define lista_2n (fila_n 1 lista))


