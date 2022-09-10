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

; Descripción: funcion que te entrega la posicion final en x
; Dom: image
; Rec: entero
(define largo_pos_x (lambda (image) (- (width_image image) 1)))

; Descripción: funcion que te entrega la posicion final en y
; Dom: image
; Rec: entero
(define largo_pos_y (lambda (image) (- (height_image image) 1)))



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
        (andmap pixrgb-d? (format_image image))))


; definir bitmap?
; Dom: image
; Rec: boleano
; Descripción: función que permite determinar si la imagen corresponde a un bitmap-d.
; Pertencencia
(define bitmap? (lambda (image)
         (andmap pixbit-d? (format_image image))))


; definir hexmap?
; Dom: Image
; Rec: Boleano
; Descripción: función que permite determinar si la imagen corresponde a un hexmap-d.
; Pertenencia
(define hexmap? (lambda (image)
          (andmap pixhex-d? (format_image image))))

; definir compressed?
; Dom: Image
; Rec: Boleano
; Descripción: función que permite determinar si la imagen sufrio una compresión o no
; Pertenencia
(define compressed? (lambda (image)
  (if (= (length (format_image image)) (* (width_image image) (height_image image)))
      #t
      #f)))

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


; Descripción: función para entregar un pixel en funcion de pos_y
; Dom: lista de pixeles, pos_y (int)
; Rec: lista
; Selector
(define obtener_pixel_y (lambda (lista pos_y)
    (if (null? lista)
        null
        (if (= pos_y (cadr(car lista)))
            (car lista)
            (obtener_pixel_y (cdr lista) pos_y)))))

;----------------------------------------- MODIFICADORES ---------------------------------------------


; Dom: imagen
; Rec: image
; Descripción: función que arregla el formato de la imagen
(define arreglar_image (lambda (image_ingresado)
        (if (= (length (format_image image_ingresado)) 1)
            (list (width_image image_ingresado) (height_image image_ingresado) (car (format_image image_ingresado)))
            image_ingresado
        )))


; Descripción: funcion que ordena una lista en funcion de y
; Dom: lista x int x image
; Rec: lista
; Modificador
(define ordenar_lista_y (lambda (lista pos_y image)
    (if (> pos_y (largo_pos_y image))
        null
        (if (< (height_image image) 2)
            lista
            (cons (obtener_pixel_y lista pos_y) (ordenar_lista_y lista (+ pos_y 1) image))))))

; Descripción: funcion que ordena el formato de la imagen por y con apoyo de ordenar_lista_y y obtener_pixel_y
; Dom: lista x image
; Rec: lista
; Modificador
(define ordenar_y (lambda (lista image)
     (ordenar_lista_y lista 0 image)))

; Descripción: funcion que modifica la posicion en y segun pos_y
; Dom: posicion_y (int) x pixel [pixrgb-d / pixhex-d / pixbit-d]
; Rec: pixel [pixrgb-d / pixhex-d / pixbit-d]
; Modificador
(define modificar_posicion_pixel_y (lambda (pos_y pixel)
     (cond
       [(pixbit-d? pixel) (cambiar_y_bit pixel pos_y)]
       [(pixhex-d? pixel) (cambiar_y_hex pixel pos_y)]
       [(pixrgb-d? pixel) (cambiar_y_rgb pixel pos_y)])))

; Descripción: funcion que te entrega una lista de las posiciones invertidas pixeles
; Dom: fila (lista) x image
; Rec: pixeles (lista)
; tipo de recursión: Natural
; Modificador
(define flipH-formato (lambda (formato_fila image)
     (define flipHN (lambda (formato_fila pos_y)
         (cond
         [(null? formato_fila) null]
         [(< pos_y 0) null]
         [else (reverse (cons (modificar_posicion_pixel_y pos_y (car formato_fila)) (flipHN (cdr formato_fila) (- pos_y 1))))
               ])))

      (flipHN formato_fila (largo_pos_x image))))


; Descripción: función que intercambia las posiciones del formato de pixeles de la imagen HORIZONTALMENTE
; Dom: image
; Rec: formato de pixeles de la image
; tipo de recursión: Natural
; Modificador
(define flipH-cambio (lambda (image)
    (define flipHC (lambda (pos_y)   
        (if (> pos_y (largo_pos_y image))
             null
             (append (ordenar_y (flipH-formato (fila_n pos_y (format_image image)) image) image) (flipHC (+ pos_y 1)))
                 )))
    (flipHC 0)))

; Descripción: función flipH, apoyada por funciones donde algunas utilizan recursión natural 
; Dom: image
; Rec: image
; Modificador
(define flipH (lambda (image_ingresado)
     (arreglar_image (image  (width_image image_ingresado) (height_image image_ingresado) (flipH-cambio image_ingresado)))))


; Descripción: funcion que modifica la posicion en x segun pos_x
; Dom: posicion_x (int) x pixel [pixrgb-d / pixhex-d / pixbit-d]
; Rec: pixel [pixrgb-d / pixhex-d / pixbit-d]
; Modificador
(define modificar_posicion_pixel_x (lambda (pos_x pixel)
     (cond
       [(pixbit-d? pixel) (cambiar_x_bit pixel pos_x)]
       [(pixhex-d? pixel) (cambiar_x_hex pixel pos_x)]
       [(pixrgb-d? pixel) (cambiar_x_rgb pixel pos_x)])))

; Descripción: función que cambiar todos los elementos x de una lista según valor n
; Dom: lista de pixeles x posicion n
; Rec: lista
; tipo de recursión: Natural
; Modificador
(define flipV-formato (lambda (fila_n n)
    (if (null? fila_n)
        null
        (cons (modificar_posicion_pixel_x n (car fila_n)) (flipV-formato (cdr fila_n) n)))))

; Descripción: función que intercambia las posiciones del formato de pixeles de la imagen VERTICALMENTE
; Dom: image
; Rec: lista
; tipo de recursión: Natural
; Modificador
(define flipV-cambio (lambda (image)
    (define flipVC (lambda (pos_x fila)
        (if (< pos_x 0)
            null
            (append (flipV-formato (fila_n fila (format_image image)) pos_x) (flipVC (- pos_x 1) (+ fila 1)))
                   )))
    (flipVC (largo_pos_y image) 0)))


; Descripción: funcion que ordena una lista en funcion de x
; Dom: lista x int x image
; Rec: lista
; tipo de recursión: Natural
(define ordenar_lista_x (lambda (lista pos_x image)
    (if (> pos_x (largo_pos_x image))
        null
        (if (< (height_image image) 2)
            lista
            (append (fila_n pos_x lista) (ordenar_lista_x lista (+ pos_x 1) image))))))

; Descripción: flipV, apoyada por funciones donde algunas utilizan recursión natural 
; Dom: image
; Rec: image
(define flipV (lambda (image_ingresado)
    (arreglar_image (image  (width_image image_ingresado) (height_image image_ingresado) (ordenar_lista_x (flipV-cambio image_ingresado) 0 image_ingresado)))))

;-------------------------------------------- OTRAS FUNCIONES----------------------------------------------

; Descripción: Histograma
; Dom: image
; Rec: lista
(define histograma (lambda (image)
    (cond
      [(pixmap? image) (histograma_rgb (format_image image))]
      [(bitmap? image) (histograma_bit (format_image image))]
      [(hexmap? image) (histograma_hex (format_image image))]
      [else image])))

; Descripción: imgRGB->imgHex
; Dom: Entero
; Rec: String
; funcion que retorna un valor entero a un string hexa
(define valor_hex (lambda (a)
   (cond
     [(= a 1) "1"]
     [(= a 2) "2"]
     [(= a 3) "3"]
     [(= a 4) "4"]
     [(= a 5) "5"]
     [(= a 6) "6"]
     [(= a 7) "7"]
     [(= a 8) "8"]
     [(= a 9) "9"]
     [(= a 10) "A"]
     [(= a 11) "B"]
     [(= a 12) "C"]
     [(= a 13) "D"]
     [(= a 14) "E"]
     [(= a 15) "F"]
     [else "0"])))

; Dom: Entero
; Rec: String
; funcion que convierte un numero a una cadena de hexa
(define rgb->hex (lambda (a)
       (string-append (valor_hex (quotient a 16)) (valor_hex (remainder a 16)))))

; Dom: 3 enteros, los colores de rgb
; Rec: String
; funcion que convierte 3 colores de rgb a un string en hexa
(define convertir_rgb (lambda (c1 c2 c3)
        (string-append (rgb->hex c1)
                       (rgb->hex c2)
                       (rgb->hex c3))))

; Dom: Lista
; Rec: Lista
; función que cambia un pixel rgb a hexa
(define convertir_rgb_hex (lambda (pixel)
       (cambiar_h_hex pixel (convertir_rgb (c1_rgb pixel) (c2_rgb pixel) (c3_rgb pixel)))))


; Dom: Image
; Rec: Image
; funcion que pasa de rgb a hexa, en caso de que no sea rgb retorna f
(define imgRGB->imgHex (lambda (image_rgb)
  (if (pixmap? image_rgb)
      (arreglar_image (image (width_image image_rgb) (height_image image_rgb) (map convertir_rgb_hex (format_image image_rgb))))    
      image_rgb))) ;si se ingresa se retorna la imagen sin cambios


(define lista_x (flipV-cambio image_2))





;;;;;;;;;;;;;;;;;;;;;;;;









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define filtro_nulos (lambda (lista)
    (if (null? lista)
        null
        (if (null? (car lista))
            null
            (cons (car lista) (filtro_nulos (cdr lista)))))
                       ))

(define lista_1 (format_image image_1))
(define lista_2 (format_image image_2))
(define lista_3 (format_image image_3))


(define lista (format_image image_2))
(define lista_n (fila_n 0 lista))
(define lista_2n (flipH-formato lista_n image_2))


