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
(define pixrgb_a (pixrgb-d 0 0 10 10 10 10)) ; lista_1
(define pixrgb_b (pixrgb-d 0 1 20 20 20 20))
(define pixrgb_c (pixrgb-d 0 2 30 30 30 30))
(define pixrgb_d (pixrgb-d 1 0 40 40 40 40))
(define pixrgb_e (pixrgb-d 1 1 50 50 50 50))
(define pixrgb_f (pixrgb-d 1 2 60 60 60 60))

; definir una image 1
(define image_1 (image 3 2 pixrgb_a pixrgb_b pixrgb_c pixrgb_d pixrgb_e pixrgb_f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; definir 4 pixeles de un pixbit-d
(define pixbit_a (pixbit-d 0 0 0 10)) ; lista_2
(define pixbit_b (pixbit-d 0 1 1 20))
(define pixbit_c (pixbit-d 0 2 0 30))
(define pixbit_d (pixbit-d 1 0 1 40))
(define pixbit_e (pixbit-d 1 1 0 50))
(define pixbit_f (pixbit-d 1 2 0 60))
(define pixbit_g (pixbit-d 2 0 0 70))
(define pixbit_h (pixbit-d 2 1 0 80))
(define pixbit_i (pixbit-d 2 2 0 90))

; definir una image 2
(define image_2 (image 3 2 pixbit_a pixbit_b pixbit_c pixbit_d pixbit_e pixbit_f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
; definir 4 pixeles de un pixhex-d
(define pixhex_a (pixhex-d 0 0 "#FF0000" 10)) ;lista_3
(define pixhex_b (pixhex-d 0 1 "#0000FF" 20))
(define pixhex_c (pixhex-d 0 2 "#00FF00" 30))
(define pixhex_d (pixhex-d 1 0 "#FFAOFF" 40))
(define pixhex_e (pixhex-d 1 1 "#FF12FF" 50))
(define pixhex_f (pixhex-d 1 2 "#F32FFF" 60))

; definir una image 3
(define image_3 (image 3 2 pixhex_a pixhex_b pixhex_c pixhex_d pixhex_e pixhex_f))

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
      (if (ormap pixbit-d_compressed? (formato_image image))
          #t
          (if (ormap pixrgb-d_compressed? (formato_image image))
              #t
              (if (ormap pixhex-d_compressed? (formato_image image))
                  #t
                  #f)))))

;----------------------------------------- SELECTORES ----------------------------------------------

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
            [(<= pos_y (largo_pos_x image)) (cons (encontrar_pixel lista pos_x pos_y) (ordenar_formato lista pos_x (+ pos_y 1) image (+ contador 1)))]
            [else (ordenar_formato lista (+ pos_x 1) 0 image contador)]))))


; Dom: imagen
; Rec: image
; Descripción: función que arregla el formato de la imagen
(define arreglar_image (lambda (image_ingresado)
        (if (= (length (formato_image image_ingresado)) 1)
            (list (ancho_image image_ingresado) (largo_image image_ingresado) (car (formato_image image_ingresado)))
            image_ingresado)))


; Descripción: función que modifica el formato de la image
(define modificar_formato_image (lambda (image_ingresada formato)
        (arreglar_image (image (ancho_image image_ingresada) (largo_image image_ingresada) formato))))


; Descripción: funcion que modifica la posicion en y e x segun pos_y y pox_x
; Dom: posicion_y (int) x posicion_x (int) x pixel [pixrgb-d / pixhex-d / pixbit-d]
; Rec: pixel [pixrgb-d / pixhex-d / pixbit-d]
; Modificador
(define modificar_posicion_pixel (lambda (pos_y pos_x pixel)
     (cond
       [(pixbit-d? pixel) (cambiar_x_bit (cambiar_y_bit pixel pos_y) pos_x)]
       [(pixhex-d? pixel) (cambiar_x_hex (cambiar_y_hex pixel pos_y) pos_x)]
       [(pixrgb-d? pixel) (cambiar_x_rgb (cambiar_y_rgb pixel pos_y) pos_x)])))

; Descripción: función flipH
; Dom: image
; Rec: image
(define flipH (lambda (image_ingresada)

    (define flipH-formato (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (and (<= fila (largo_pos_x image)) (>= contador 0))
                 (cons (modificar_posicion_pixel contador fila (car formato_pixeles)) (flipH-formato (cdr formato_pixeles) fila (- contador 1) image)) 
                  (if (<= fila (largo_pos_x image))
                      (flipH-formato formato_pixeles (+ fila 1) (largo_pos_x image_ingresada) image)
                       null)))))

   (modificar_formato_image image_ingresada (ordenar_formato (flipH-formato (formato_image image_ingresada) 0 (largo_pos_x image_ingresada)  image_ingresada) 0 0 image_ingresada 0))))


; Descripción: función flipV
; Dom: image
; Rec:
(define flipV (lambda (image_ingresada)

    (define flipV-formato (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (and (>= fila 0) (<= contador (largo_pos_x image)))
                 (cons (modificar_posicion_pixel contador fila (car formato_pixeles)) (flipV-formato (cdr formato_pixeles) fila (+ contador 1) image)) 
                  (if (>= fila 0)
                      (flipV-formato formato_pixeles (- fila 1) 0 image)
                       null)))))

     (modificar_formato_image image_ingresada (ordenar_formato (flipV-formato (formato_image image_ingresada) (largo_pos_y image_ingresada) 0 image_ingresada) 0 0 image_ingresada 0))))

; Descripción: función rotate90
; Dom: image
; Rec: image
(define rotate90 (lambda (image_ingresada)
                   
    (define rotate (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (and (>= fila 0) (<= contador (largo_pos_x image)))
                    (cons (modificar_posicion_pixel fila contador (car formato_pixeles)) (rotate (cdr formato_pixeles) fila (+ contador 1) image))
                    (if (>= fila 0)
                        (rotate formato_pixeles (- fila 1) 0 image)
                        null)))))

    (define intercambiar_dimensiones (lambda (image_ingresada)
          (image (largo_image image_ingresada) (ancho_image image_ingresada) (formato_image image_ingresada))))

    (modificar_formato_image (intercambiar_dimensiones image_ingresada) (ordenar_formato (rotate (formato_image image_ingresada) (largo_pos_y image_ingresada) 0 image_ingresada) 0 0 (intercambiar_dimensiones image_ingresada) 0))))


;-------------------------------------------- OTRAS FUNCIONES----------------------------------------------

; Descripción: Histograma
; Dom: image
; Rec: lista
(define histogram (lambda (image)
    (cond
      [(pixmap? image) (histograma_rgb (formato_image image))]
      [(ormap pixrgb-d_compressed? (formato_image image)) (histograma_rgb (formato_image image))]
      [(bitmap? image) (histograma_bit (formato_image image))]
      [(ormap pixbit-d_compressed? (formato_image image)) (histograma_bit (formato_image image))]
      [(hexmap? image) (histograma_hex (formato_image image))]
      [(ormap pixhex-d_compressed? (formato_image image)) (histograma_hex (formato_image image))]
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

   (cambiar_d_hex (cambiar_h_hex pixel (convertir_rgb (c1_rgb pixel) (c2_rgb pixel) (c3_rgb pixel))) (d_rgb pixel))))


; Dom: Image
; Rec: Image
; funcion que pasa de rgb a hexa, en caso de que no sea rgb retorna f
(define imgRGB->imgHex (lambda (image_rgb)
  (if (pixmap? image_rgb)
      (modificar_formato_image image_rgb (map convertir_rgb_hex (formato_image image_rgb)))    
      image_rgb))) ;si se ingresa una imagen distinta a rgb se retorna la imagen sin cambios


; Descripción: crop
; Dom: image X x1 X y1 X x2 X y2
; Rec: image
(define crop (lambda (image_ingresada x1 y1 x2 y2)               
    (define crop_formato (lambda (formato_pixeles x1 y1 x2 y2)
        (if (null? formato_pixeles)
            null
            (if (and
                 (>= (x_rgb (car formato_pixeles)) x1)
                 (<= (x_rgb (car formato_pixeles)) x2)
                 (>= (y_rgb (car formato_pixeles)) y1)
                 (<= (y_rgb (car formato_pixeles)) y2))
                
                 (cons (car formato_pixeles) (crop_formato (cdr formato_pixeles) x1 y1 x2 y2))
                 (crop_formato (cdr formato_pixeles) x1 y1 x2 y2)))))

     (modificar_formato_image image_ingresada (crop_formato (formato_image image_ingresada) (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))))


; Descripción: Compress
; Dom: image
; Rec: image
(define compress (lambda (image_ingresada . vacio)
    
    (cond
      [(bitmap? image_ingresada)
       
       (let
        [(x (bit_mayor (histogram image_ingresada)))]
         (let
             [(y (modificar_formato_image image_ingresada
                        (compress-formato-bit (formato_image image_ingresada) (bit_mayor (histogram image_ingresada)))))]
            (if (null? vacio)
                y
                x)))]
         
         
      [(hexmap? image_ingresada)
       (modificar_formato_image image_ingresada
                        (compress-formato-hex (formato_image image_ingresada) (hex_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))]

      [(pixmap? image_ingresada)
       (modificar_formato_image image_ingresada
                        (compress-formato-rgb (formato_image image_ingresada) (rgb_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))]

)))
      
; Descripción: Descompress
; Dom: image
; Rec: image

(define descompress (lambda (image_ingresada)

                      
    (cond
      [(ormap pixbit-d_compressed? (formato_image image))
       (modificar_formato_image
        (descompress-formato-bit (formato_image image_ingresada) (bit_mayor (histogram image_ingresada))))]
      

      [(ormap pixhex-d_compressed? (formato_image image)) (modificar_formato_image (descompress-formato-hex (formato_image image_ingresada) (hex_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))]



      [(ormap pixrgb-d_compressed? (formato_image image)) (modificar_formato_image image)]
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
       (modificar_formato_image image_ingresado (map_edit filtro (formato_image image_ingresado)))))



; Descripción: image->string
; Dom: image x función
; Rec: string
(define image->string (lambda (image funcion_pixel)
        (funcion_pixel (formato_image image) (largo_pos_y image))))


;-----------------------------------------------------------------


; exportar la funcion al exterior
(provide (all-defined-out))
