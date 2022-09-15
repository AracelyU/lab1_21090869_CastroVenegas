#lang racket

; Tda externos utilizados
(require "TDA_Pixrgb-d.rkt")
(require "TDA_Pixbit-d.rkt")
(require "TDA_Pixhex-d.rkt")



#|
-------------------------------- TDA IMAGEN --------------------------------------------------------------
 
-------------------------------- REPRESENTACION ----------------------------------------------------------

 El TDA representa una imagen como una lista de tres parametros, uno que contiene el ancho, otro el
 largo y un formato de pixeles que pueden ser variable y del tipo pixrgb-d, pixbit-d o pixhex-d
(int X int X list)

|#

;------------------------------- CONSTRUCTOR -------------------------------------------------------------

; Dominio: ancho (int) x largo (int) x formato de pixeles (list) 
; Recorrido: image
; Descripción: Función constructora de imágenes
(define image (lambda (ancho largo . formato) (list ancho largo formato)))

;------------------------------- SELECTORES --------------------------------------------------------------

; Dominio: image
; Recorrido: ancho (int)
; Descripción: Función que recupera el ancho de la imagen
(define width-image (lambda (L) (car L)))

; Dominio: image
; Recorrido: largo (int)
; Descripción: Función que recupera el largo de la imagen
(define height-image (lambda (L) (cadr L)))

; Dominio: image
; Recorrido: formato de pixeles (list)
; Descripción: Función que recupera el formato de pixeles de la imagen
(define pixel-format (lambda (L) (caddr L))) ; tercera posición (lista)

; Dominio: image
; Recorrido: int
; Descripción: Función que recupera la posición final en x
(define largo_pos_x (lambda (image) (- (width-image image) 1)))

; Dominio: image
; Recorrido: entero
; Descripción: Función que recupera la posición final en y
(define largo_pos_y (lambda (image) (- (height-image image) 1)))


; Dominio: formato de pixeles (list), posicion_x (int), posicion_y (int)
; Recorrido: pixel
; Descripción: Función que recupera un pixel del formato pixeles según coordenadas (x, y)
(define encontrar_pixel (lambda (lista pos_x pos_y)
    (if (null? lista)
        null
        (if (and (= (car (car lista)) pos_x) (= (cadr (car lista)) pos_y))
            (car lista)
            (encontrar_pixel (cdr lista) pos_x pos_y)))))


;----------------------------- PERTENENCIA --------------------------------------------------------------------

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen corresponde a un pixmap-d.
(define pixmap? (lambda (image) (andmap pixrgb-d? (pixel-format image))))

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen corresponde a un bitmap-d.
(define bitmap? (lambda (image) (andmap pixbit-d? (pixel-format image))))

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen corresponde a un hexmap-d.
(define hexmap? (lambda (image) (andmap pixhex-d? (pixel-format image))))

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen sufrio una compresión
(define compressed? (lambda (image)
      (if (ormap pixbit-d_compressed? (pixel-format image))
          #t
          (if (ormap pixrgb-d_compressed? (pixel-format image))
              #t
              (if (ormap pixhex-d_compressed? (pixel-format image))
                  #t
                  (if (or (bitmap? image) (pixmap? image) (hexmap? image))
                      #f
                      #t))))))

;----------------------------------------- MODIFICADORES ---------------------------------------------

; Dominio: formato de pixeles (list) X posición_x (int) X posición_y (int) X image_ingresada (image) X contador (int)
; Recorrido: formato de pixeles (list)
; Descripción: Función que ordena un formato de pixeles
; Tipo de recursión: Natural
(define ordenar_formato (lambda (lista pos_x pos_y image contador)
      (if (= contador (* (width-image image) (height-image image)))
          null
          (cond
            [(<= pos_y (largo_pos_x image)) (cons (encontrar_pixel lista pos_x pos_y) (ordenar_formato lista pos_x (+ pos_y 1) image (+ contador 1)))]
            [else (ordenar_formato lista (+ pos_x 1) 0 image contador)]))))

; Dominio: image
; Recorrido: image
; Descripción: Función que arregla el formato de la imagen
(define arreglar_image (lambda (image_ingresada)
        (if (= (length (pixel-format image_ingresada)) 1)
            (list (width-image image_ingresada) (height-image image_ingresada) (car (pixel-format image_ingresada)))
            image_ingresada)))

; Dominio: image
; Recorrido: image
; Descripción: Función que modifica el formato de la image
(define modificar_formato_image (lambda (image_ingresada formato)
        (arreglar_image (image (width-image image_ingresada) (height-image image_ingresada) formato))))

; Dominio: posición_y (int) X posición_x (int) X pixel [pixrgb-d / pixhex-d / pixbit-d]
; Recorrido: pixel
; Descripción: Función que modifica la posicion en y e x según coordenadas (x, y)
(define modificar_posicion_pixel (lambda (pos_y pos_x pixel)
     (cond
       [(pixbit-d? pixel) (cambiar_x_bit (cambiar_y_bit pixel pos_y) pos_x)]
       [(pixhex-d? pixel) (cambiar_x_hex (cambiar_y_hex pixel pos_y) pos_x)]
       [(pixrgb-d? pixel) (cambiar_x_rgb (cambiar_y_rgb pixel pos_y) pos_x)])))

; Dominio: image
; Recorrido: image
; Descripción: Función que invierte la imagen horizontalmente, flipH
; Tipo de recursión: Natural
(define flipH (lambda (image_ingresada)

    ; Función que cambia el formato de pixeles de forma que se invierta horizontalmente
    (define flipH-formato (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (and (<= fila (largo_pos_x image)) (>= contador 0))
                 (cons (modificar_posicion_pixel contador fila (car formato_pixeles)) (flipH-formato (cdr formato_pixeles) fila (- contador 1) image)) 
                  (if (<= fila (largo_pos_x image))
                      (flipH-formato formato_pixeles (+ fila 1) (largo_pos_x image_ingresada) image)
                       null)))))

   (modificar_formato_image image_ingresada (ordenar_formato (flipH-formato (pixel-format image_ingresada) 0 (largo_pos_x image_ingresada)  image_ingresada) 0 0 image_ingresada 0))))

; Dominio: image
; Recorrido: image
; Descripción: Función que invierte la imagen verticalmente, flipV
; Tipo de recursión: Natural
(define flipV (lambda (image_ingresada)

    ; Función que cambia el formato de pixeles de forma que se invierta verticalmente
    (define flipV-formato (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (and (>= fila 0) (<= contador (largo_pos_x image)))
                 (cons (modificar_posicion_pixel contador fila (car formato_pixeles)) (flipV-formato (cdr formato_pixeles) fila (+ contador 1) image)) 
                  (if (>= fila 0)
                      (flipV-formato formato_pixeles (- fila 1) 0 image)
                       null)))))

     (modificar_formato_image image_ingresada (ordenar_formato (flipV-formato (pixel-format image_ingresada) (largo_pos_y image_ingresada) 0 image_ingresada) 0 0 image_ingresada 0))))

; Dominio: image
; Recorrido: image
; Descripción: Función que rota la imagen 90° a la derecha, rotate90
; Tipo de recursión: Natural
(define rotate90 (lambda (image_ingresada)

    ; Función que cambia el formato de pixeles de forma que rote 90° a la derecha
    (define rotate (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (and (>= fila 0) (<= contador (largo_pos_x image)))
                    (cons (modificar_posicion_pixel fila contador (car formato_pixeles)) (rotate (cdr formato_pixeles) fila (+ contador 1) image))
                    (if (>= fila 0)
                        (rotate formato_pixeles (- fila 1) 0 image)
                        null)))))

    ; Función que intercambia el ancho con el largo para cuando rotas la imagen
    (define intercambiar_dimensiones (lambda (image_ingresada) (image (height-image image_ingresada) (width-image image_ingresada) (pixel-format image_ingresada))))

    (modificar_formato_image (intercambiar_dimensiones image_ingresada)
                             (ordenar_formato (rotate (pixel-format image_ingresada) (largo_pos_y image_ingresada) 0 image_ingresada) 0 0 (intercambiar_dimensiones image_ingresada) 0))))

;-------------------------------------------- OTRAS FUNCIONES----------------------------------------------

; Dominio: image
; Recorrido: formato de pixeles (list)
; Descripción: Función que retorna un histograma de frecuencias a partir de los colores de una imagen, histogram
; Comentario: Se agrega el histograma para una imagen comprimida bitmap-d para la función descompress más adelante
(define histogram (lambda (image)
    (cond
      [(pixmap? image) (histograma_rgb (pixel-format image))]
      [(bitmap? image) (histogram_bit (pixel-format image))]
      [(hexmap? image) (histograma_hex (pixel-format image))]
      [(ormap pixbit-d_compressed? (pixel-format image)) (histogram_bit (pixel-format image))]
      [else image])))


; Dominio: pixel
; Recorrido: string
; Descripción: Función que cambia un pixmap a hexmap
(define convertir_rgb_hex (lambda (pixel)
                            
    ; Función para convertir un número a hexadecimal
    (define valor_hex (lambda (a)
       (cond   [(= a 1) "1"][(= a 2) "2"][(= a 3) "3"][(= a 4) "4"]
               [(= a 5) "5"][(= a 6) "6"][(= a 7) "7"][(= a 8) "8"]
               [(= a 9) "9"][(= a 10) "A"][(= a 11) "B"][(= a 12) "C"]
               [(= a 13) "D"][(= a 14) "E"][(= a 15) "F"][else "0"])))

    ; Función que transforma un color a hexadecimal
    (define rgb->hex (lambda (a) (string-append (valor_hex (quotient a 16)) (valor_hex (remainder a 16)))))                 

    ; Función que tranforma los colores de pixmap a hexmap
    (define convertir_rgb (lambda (c1 c2 c3) (string-append (rgb->hex c1)(rgb->hex c2)(rgb->hex c3))))

    (cambiar_d_hex (cambiar_h_hex pixel (convertir_rgb (getR pixel) (getG pixel) (getB pixel))) (getD pixel))))

; Dominio: image
; Recorrido: image
; Descripción: Función que convierte una imagen pixmap-d a hexmap-d
(define imgRGB->imgHex (lambda (image_rgb)
  (if (pixmap? image_rgb)
      (modificar_formato_image image_rgb (map convertir_rgb_hex (pixel-format image_rgb)))    
      image_rgb)))

; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
; Descripción: Función que recorta una imagen a partir de un cuadrante, crop
; Tipo de recursión: Natural
(define crop (lambda (image_ingresada x1 y1 x2 y2)

    ; Función que crea el formato con pixeles cuyas coordenadas están dentro del cuadrante
    (define crop_formato (lambda (formato_pixeles x1 y1 x2 y2)
        (if (null? formato_pixeles)
            null
            (if (and (>= (x_rgb (car formato_pixeles)) x1) (<= (x_rgb (car formato_pixeles)) x2) (>= (y_rgb (car formato_pixeles)) y1) (<= (y_rgb (car formato_pixeles)) y2))
                 (cons (car formato_pixeles) (crop_formato (cdr formato_pixeles) x1 y1 x2 y2))
                 (crop_formato (cdr formato_pixeles) x1 y1 x2 y2)))))

     (modificar_formato_image image_ingresada (crop_formato (pixel-format image_ingresada) (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))))


; Dominio: image
; Recorrido: image
; Descripción: Función que comprime una imagen eliminando aquellos pixeles con el color más frecuente, compress
(define compress (lambda (image_ingresada)
    
    (cond     [(bitmap? image_ingresada) (modificar_formato_image image_ingresada
                        (compress-formato-bit (pixel-format image_ingresada) (bit_mayor (histogram image_ingresada))))]
         
              [(hexmap? image_ingresada) (modificar_formato_image image_ingresada
                        (compress-formato-hex (pixel-format image_ingresada) (hex_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))]

              [(pixmap? image_ingresada) (modificar_formato_image image_ingresada
                        (compress-formato-rgb (pixel-format image_ingresada) (rgb_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))])))

; Dominio: image
; Recorrido: image
; Descripción: Función que permite descomprimir una imagen comprimida, descompress
(define descompress (lambda (image_ingresada)
                      
    (cond  [(ormap pixbit-d_compressed? (pixel-format image_ingresada)) (modificar_formato_image image_ingresada
                       (descompress-formato-bit (pixel-format image_ingresada) (bit_menor (histogram image_ingresada))))]
           
           [(ormap pixhex-d_compressed? (pixel-format image_ingresada)) (modificar_formato_image image_ingresada
                       (descompress-formato-hex (pixel-format image_ingresada)))]
           
           [(ormap pixrgb-d_compressed? (pixel-format image_ingresada)) (modificar_formato_image image_ingresada
                       (descompress-formato-rgb (pixel-format image_ingresada)))]
           
           [else image_ingresada])))

; Dominio: image
; Recorrido: image
; Descripción: Función que permite aplicar funciones especiales a las imágenes, edit
; Tipo de recursión: Natural
(define edit (lambda (filtro image_ingresado)
     (define map_edit (lambda (filtro lista)
       (if (null? lista)
           null
           (cons (filtro (car lista)) (map_edit filtro (cdr lista))))))
       (modificar_formato_image image_ingresado (map_edit filtro (pixel-format image_ingresado)))))

; Dominio: image X función
; Recorrido: string
; Descripción: Función que transforma una imagen a una representación string, image->string
(define image->string (lambda (image funcion_pixel)
        (funcion_pixel (pixel-format image) (largo_pos_y image))))






; exportar la funcion al exterior
(provide (all-defined-out))
