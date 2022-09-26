#lang racket

; Tda externos utilizados
(require "TDA_Pixrgb-d_21090869_CastroVenegas.rkt")
(require "TDA_Pixbit-d_21090869_CastroVenegas.rkt")
(require "TDA_Pixhex-d_21090869_CastroVenegas.rkt")

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
(define image (lambda (ancho largo . formato) (list ancho largo formato))) ; requerimiento funcional

;----------------------------- PERTENENCIA --------------------------------------------------------------------

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen corresponde a un pixmap-d.
(define pixmap? (lambda (image) (andmap pixrgb-d? (pixel_formato image)))) ; requerimiento funcional

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen corresponde a un bitmap-d.
(define bitmap? (lambda (image) (andmap pixbit-d? (pixel_formato image)))) ; requerimiento funcional
 
; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen corresponde a un hexmap-d.
(define hexmap? (lambda (image) (andmap pixhex-d? (pixel_formato image)))) ; requerimiento funcional

; Dominio: image
; Recorrido: boleano
; Descripción: Función que permite determinar si la imagen sufrio una compresión
(define compressed? (lambda (image) ; requerimiento funcional
      (if (ormap pixbit-d_compressed? (pixel_formato image))
          #t
          (if (ormap pixrgb-d_compressed? (pixel_formato image))
              #t
              (if (ormap pixhex-d_compressed? (pixel_formato image))
                  #t
                  (if (or (bitmap? image) (pixmap? image) (hexmap? image))
                      #f
                      #t))))))

; Dominio: formato de pixeles (list) X  int X int
; Recorrido: boleano
; Descripción: Función que permite un pixel existe en una imagen
; Tipo de recursión: Natural
(define encontrar_pixel? (lambda (lista pos_x pos_y) ; función adicional
    (if (null? lista)
        #f
        (if (and (not (null? (car lista))) (= (x_rgb (car lista)) pos_x) (= (y_rgb (car lista)) pos_y))
            #t
            (encontrar_pixel? (cdr lista) pos_x pos_y)))))


;------------------------------- SELECTORES --------------------------------------------------------------

; Dominio: image
; Recorrido: ancho (int)
; Descripción: Función que recupera el ancho de la imagen
(define ancho_image (lambda (L) (car L))) ; función adicional

; Dominio: image
; Recorrido: largo (int)
; Descripción: Función que recupera el largo de la imagen
(define largo_image (lambda (L) (cadr L))) ; función adicional

; Dominio: image
; Recorrido: formato de pixeles (list)
; Descripción: Función que recupera el formato de pixeles de la imagen
(define pixel_formato (lambda (L) (caddr L))) ; función adicional

; Dominio: image
; Recorrido: int
; Descripción: Función que recupera la posición final en x
(define largo_pos_x (lambda (image) (- (ancho_image image) 1))) ; función adicional

; Dominio: image
; Recorrido: entero
; Descripción: Función que recupera la posición final en y
(define largo_pos_y (lambda (image) (- (largo_image image) 1))) ; función adicional


; Dominio: formato de pixeles (list), posicion_x (int), posicion_y (int)
; Recorrido: pixel
; Descripción: Función que recupera un pixel del formato pixeles según coordenadas (x, y)
(define encontrar_pixel (lambda (lista pos_x pos_y) ; función adicional
    (if (null? lista)
        null
        (if (and (= (x_rgb (car lista)) pos_x) (= (y_rgb (car lista)) pos_y))
            (car lista)
            (encontrar_pixel (cdr lista) pos_x pos_y)))))


;----------------------------------------- MODIFICADORES ---------------------------------------------
; Dominio: formato de pixeles (list) X posición_x (int) X posición_y (int) X image_ingresada (image) X contador (int)
; Recorrido: formato de pixeles (list)
; Descripción: Función que ordena un formato de pixeles
; Tipo de recursión: Natural
(define ordenar_formato (lambda (lista pos_x pos_y image contador) ; función adicional
                          
      (if (or (= contador (* (ancho_image image) (largo_image image))) (= contador (length lista)))
             null
            (cond
                [(not (encontrar_pixel? lista pos_x pos_y))
                     (if (< pos_y (largo_image image))
                         (ordenar_formato lista pos_x (+ pos_y 1) image contador)
                         (ordenar_formato lista (+ pos_x 1) 0 image contador))]
                
                [(< pos_y (largo_image image))
                     (cons (encontrar_pixel lista pos_x pos_y) (ordenar_formato lista pos_x (+ pos_y 1) image (+ contador 1)))]

                [else (ordenar_formato lista (+ pos_x 1) 0 image contador)]))))

; Dominio: image
; Recorrido: image
; Descripción: Función que modifica el formato de la image
(define modificar_formato_image (lambda (image_ingresada formato) ; función adicional

        ; Función que arregla el formato de una imagen
        (define arreglar_image (lambda (image_ingresada)
        (if (= (length (pixel_formato image_ingresada)) 1)
            (list (ancho_image image_ingresada) (largo_image image_ingresada) (car (pixel_formato image_ingresada))) 
            image_ingresada)))
                     
        (arreglar_image (image (ancho_image image_ingresada) (largo_image image_ingresada) formato))))

; Dominio: posición_y (int) X posición_x (int) X pixel [pixrgb-d / pixhex-d / pixbit-d]
; Recorrido: pixel
; Descripción: Función que modifica la posicion en y e x según coordenadas (x, y)
(define modificar_posicion_pixel (lambda (pos_y pos_x pixel) ; función adicional
     (cond
       [(pixbit-d? pixel) (cambiar_x_bit (cambiar_y_bit pixel pos_y) pos_x)]
       [(pixhex-d? pixel) (cambiar_x_hex (cambiar_y_hex pixel pos_y) pos_x)]
       [(pixrgb-d? pixel) (cambiar_x_rgb (cambiar_y_rgb pixel pos_y) pos_x)])))

; Dominio: image
; Recorrido: image
; Descripción: Función que invierte la imagen horizontalmente, flipH
; Tipo de recursión: Natural
(define flipH (lambda (image_ingresada) ; requerimiento funcional
 
    ; Función que cambia el formato de pixeles de forma que se invierta horizontalmente
    (define flipH-formato (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
             null
            (if (not (encontrar_pixel? formato_pixeles fila (- (largo_pos_y image) contador)))
                 (if (>= contador 0)
                      (flipH-formato formato_pixeles fila (- contador 1) image)
                      (if (<= fila (largo_pos_x image))
                           (flipH-formato formato_pixeles (+ fila 1) (largo_pos_y image_ingresada) image)
                            null))
                 
                 (if (>= contador 0)
                     (cons (modificar_posicion_pixel contador fila (car formato_pixeles)) (flipH-formato (cdr formato_pixeles) fila (- contador 1) image)) 
                     (if (<= fila (largo_pos_x image))
                          (flipH-formato formato_pixeles (+ fila 1) (largo_pos_y image_ingresada) image)
                           null))))))
                
   (modificar_formato_image image_ingresada 
          (ordenar_formato (flipH-formato (pixel_formato image_ingresada) 0 (largo_pos_y image_ingresada) image_ingresada) 0 0 image_ingresada 0))))


; Dominio: image
; Recorrido: image
; Descripción: Función que invierte la imagen verticalmente, flipV
; Tipo de recursión: Natural
(define flipV (lambda (image_ingresada) ; requerimiento funcional

    ; Función que cambia el formato de pixeles de forma que se invierta verticalmente
    (define flipV-formato (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null

            (if (not (encontrar_pixel? formato_pixeles (- (largo_pos_x image) fila) contador))
               (if (<= contador (largo_pos_y image))
                     (flipV-formato formato_pixeles fila (+ contador 1) image)
                     (if (>= fila 0)
                         (flipV-formato formato_pixeles (- fila 1) 0 image)
                          null))

                (if (<= contador (largo_pos_y image))
                     (cons (modificar_posicion_pixel contador fila (car formato_pixeles)) (flipV-formato (cdr formato_pixeles) fila (+ contador 1) image)) 
                     (if (>= fila 0)
                         (flipV-formato formato_pixeles (- fila 1) 0 image)
                          null))))))

     (modificar_formato_image image_ingresada (ordenar_formato
              (flipV-formato (pixel_formato image_ingresada) (largo_pos_x image_ingresada) 0 image_ingresada) 0 0 image_ingresada 0))))


; Dominio: image
; Recorrido: image
; Descripción: Función que rota la imagen 90° a la derecha, rotate90
; Tipo de recursión: Natural
(define rotate90 (lambda (image_ingresada) ; requerimiento funcional

    ; Función que cambia el formato de pixeles de forma que rote 90° a la derecha
    (define rotate (lambda (formato_pixeles fila contador image)
        (if (null? formato_pixeles)
            null
            (if (not (encontrar_pixel? formato_pixeles (- (largo_pos_x image) fila) contador))
                  (if (<= contador (largo_pos_y image))
                      (rotate formato_pixeles fila (+ contador 1) image)
                      (if (>= fila 0)
                           (rotate formato_pixeles (- fila 1) 0 image)
                            null))
                 (if (<= contador (largo_pos_y image))
                      (cons (modificar_posicion_pixel fila contador (car formato_pixeles)) (rotate (cdr formato_pixeles) fila (+ contador 1) image))
                      (if (>= fila 0)
                          (rotate formato_pixeles (- fila 1) 0 image)
                           null))))))

    ; Función que intercambia el ancho con el largo para cuando rotas la imagen
    (define intercambiar_dimensiones (lambda (image_ingresada)
            (image (largo_image image_ingresada) (ancho_image image_ingresada) (pixel_formato image_ingresada))))

    (modificar_formato_image (intercambiar_dimensiones image_ingresada) (ordenar_formato
           (rotate (pixel_formato image_ingresada) (largo_pos_x image_ingresada) 0 image_ingresada) 0 0 (intercambiar_dimensiones image_ingresada) 0))))

;-------------------------------------------- OTRAS FUNCIONES----------------------------------------------

; Dominio: image
; Recorrido: formato de pixeles (list)
; Descripción: Función que retorna un histograma de frecuencias a partir de los colores de una imagen, histogram
(define histogram (lambda (image) ; requerimiento funcional
    (cond
      [(pixmap? image) (histograma_rgb (pixel_formato image))]
      [(bitmap? image) (histogram_bit (pixel_formato image))]
      [(hexmap? image) (histograma_hex (pixel_formato image))]
      [else image])))

; Dominio: image
; Recorrido: image
; Descripción: Función que convierte una imagen pixmap-d a hexmap-d
(define imgRGB->imgHex (lambda (image_rgb) ; requerimiento funcional

    ; Función que cambia un pixmap a hexmap, Dominio: pixel, Recorrido: pixel
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
    (define convertir_rgb (lambda (c1 c2 c3) (string-append "#" (rgb->hex c1)(rgb->hex c2)(rgb->hex c3))))

    (cambiar_d_hex (cambiar_h_hex pixel (convertir_rgb (getR pixel) (getG pixel) (getB pixel))) (getD pixel))))
                         
    (if (pixmap? image_rgb)
        (modificar_formato_image image_rgb (map convertir_rgb_hex (pixel_formato image_rgb)))    
         image_rgb)))

; Dominio: image X x1 (int) X y1 (int) X x2 (int) X y2 (int)
; Recorrido: image
; Descripción: Función que recorta una imagen a partir de un cuadrante, crop
; Tipo de recursión: Natural
(define crop (lambda (image_ingresada x1 y1 x2 y2) ; requerimiento funcional

    ; Función que crea el formato con pixeles cuyas coordenadas están dentro del cuadrante
    (define crop_formato (lambda (formato_pixeles x1 y1 x2 y2)
        (if (null? formato_pixeles)
            null
            (if (and (>= (x_rgb (car formato_pixeles)) x1) (<= (x_rgb (car formato_pixeles)) x2) (>= (y_rgb (car formato_pixeles)) y1) (<= (y_rgb (car formato_pixeles)) y2))
                 (cons (car formato_pixeles) (crop_formato (cdr formato_pixeles) x1 y1 x2 y2))
                 (crop_formato (cdr formato_pixeles) x1 y1 x2 y2)))))

     (modificar_formato_image image_ingresada (crop_formato (pixel_formato image_ingresada) (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))))


; Dominio: image
; Recorrido: image
; Descripción: Función que comprime una imagen eliminando aquellos pixeles con el color más frecuente, compress
(define compress (lambda (image_ingresada) ; requerimiento funcional

    (define bit_mayor (lambda (lista_bit)
        (if (> (car (car lista_bit)) (car (car (cdr lista_bit)))) 0 1)))

    (define rgb_mayor (lambda (lista_rgb result)
    (if (null? lista_rgb)
        (car (cdr result))
        (if (> (car(car lista_rgb)) (car result))
            (rgb_mayor (cdr lista_rgb) (car lista_rgb))
            (rgb_mayor (cdr lista_rgb) result)))))


     (define hex_mayor (lambda (lista_hex result)
    (if (null? lista_hex)
        (car (cdr result))
        (if (> (car(car lista_hex)) (car result))
            (hex_mayor (cdr lista_hex) (car lista_hex))
            (hex_mayor (cdr lista_hex) result)))))
                   
                   
    
    (cond     [(bitmap? image_ingresada) (modificar_formato_image image_ingresada
                        (compress-formato-bit (pixel_formato image_ingresada) (bit_mayor (histogram image_ingresada))))]
         
              [(hexmap? image_ingresada) (modificar_formato_image image_ingresada
                        (compress-formato-hex (pixel_formato image_ingresada) (hex_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))]

              [(pixmap? image_ingresada) (modificar_formato_image image_ingresada
                        (compress-formato-rgb (pixel_formato image_ingresada) (rgb_mayor (histogram image_ingresada) (car (histogram image_ingresada)))))])))

; Dominio: image
; Recorrido: image
; Descripción: Función que permite descomprimir una imagen comprimida, descompress
(define decompress (lambda (image_ingresada) ; requerimiento funcional

    (define bit_faltante (lambda (formato_pixeles result)
            (if (null? formato_pixeles)
                result
                (if (= (bit (car formato_pixeles)) -1)
                    (bit_faltante (cdr formato_pixeles) result)
                    (if (= (bit (car formato_pixeles)) 0)
                        (bit_faltante (cdr formato_pixeles) 1)
                        (bit_faltante (cdr formato_pixeles 0)))))))
                     
                      
    (cond  [(ormap pixbit-d_compressed? (pixel_formato image_ingresada)) (modificar_formato_image image_ingresada
                       (descompress-formato-bit (pixel_formato image_ingresada) (bit_faltante (pixel_formato image_ingresada) 0)))]
           
           [(ormap pixhex-d_compressed? (pixel_formato image_ingresada)) (modificar_formato_image image_ingresada
                       (descompress-formato-hex (pixel_formato image_ingresada)))]
           
           [(ormap pixrgb-d_compressed? (pixel_formato image_ingresada)) (modificar_formato_image image_ingresada
                       (descompress-formato-rgb (pixel_formato image_ingresada)))]
           
           [else image_ingresada])))

; Dominio: image
; Recorrido: image
; Descripción: Función que permite aplicar funciones especiales a las imágenes, edit
; Tipo de recursión: Natural
(define edit (lambda (filtro image_ingresado) ; requerimiento funcional
       (define map_edit (lambda (filtro lista)
         (if (null? lista)
           null
          (cons (filtro (car lista)) (map_edit filtro (cdr lista))))))
       (modificar_formato_image image_ingresado (map_edit filtro (pixel_formato image_ingresado)))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que invierte el color de un pixrgb-d, invertColorRGB
(define invertColorRGB (lambda (pixrgb-d_pasado)
     (define invertir_color (lambda (color)
           (abs (- color 255))))

     (if (pixrgb-d? pixrgb-d_pasado)
        (setB (setG (setR pixrgb-d_pasado (invertir_color (getR pixrgb-d_pasado))) (invertir_color (getG pixrgb-d_pasado))) (invertir_color (getB pixrgb-d_pasado)))
        pixrgb-d_pasado)))

; Dominio: pixbit-d
; Recorrido: pixbit-d
; Descripción: Función que invierte el color de bit de un pixbit-d
(define invertColorBit (lambda (pixbit-d_pasado)
    (if (pixbit-d? pixbit-d_pasado)
        (if (= (bit pixbit-d_pasado) 0)
            (cambiar_b_bit pixbit-d_pasado 1)
            (cambiar_b_bit pixbit-d_pasado 0))
        pixbit-d_pasado)))


; Dominio: image X función
; Recorrido: string
; Descripción: Función que transforma una imagen a una representación string, image->string
(define image->string (lambda (image funcion_pixel) ; requerimiento funcional

      (define crop_formato (lambda (lista pos_x pos_y contador image)
        (if (= contador (* (ancho_image image) (largo_image image)))
             null
            (cond
                [(not (encontrar_pixel? lista pos_x pos_y))
                     (if (< pos_y (largo_image image))
                         (cons null (crop_formato lista pos_x (+ pos_y 1) (+ contador 1) image))
                         (crop_formato lista (+ pos_x 1) 0 contador image))]
                
                [(< pos_y (largo_image image))
                     (cons (encontrar_pixel lista pos_x pos_y) (crop_formato lista pos_x (+ pos_y 1) (+ contador 1) image))]

                [else (crop_formato lista (+ pos_x 1) 0 contador image)]))))

        (if (compressed? image)
            (funcion_pixel (crop_formato (pixel_formato (decompress image)) 0 0 0 image) image)
            (funcion_pixel (crop_formato (pixel_formato image) 0 0 0 image) image))

))

; Dominio: función_selectora X función_modificadora X función_operación X pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica un canal de colores o profundidad
(define adjustChannel (lambda (funcion_get funcion_set funcion_operacion)
                      (lambda (pixrgb-d_pasado) (funcion_set pixrgb-d_pasado (funcion_operacion (funcion_get pixrgb-d_pasado))))))

; Dominio: int
; Recorrido: int
; Descripción: Función que aumenta el canal en uno
(define incCh (lambda (entero) (+ entero 1)))

; Dominio: image
; Recorrido: image (list)
; Descripción: Función que permite crear una imágen de una profundidad rellenandolo con blanco
; Tipo de recursión: Natural
(define depthLayers (lambda (image_ingresada) ; requerimiento funcional
  
   ; Función que crea la lista de imagenes de profundidad con bitmap y hexmap  
   (define profundidad_bit_hex (lambda (image_ingresada lista reemplazo)

     ; Función que rellena el resto de pixeles en blanco (0 - "#000000")
     (define rellenar_profundidad (lambda (lista pos_x pos_y image contador elemento reemplazo)
          (if (or (null? lista) (= contador (* (ancho_image image) (largo_image image))))
              null
             (cond
                     [(null? (encontrar_pixel lista pos_x pos_y))
                        (if (<= pos_y (largo_pos_x image))
                            (rellenar_profundidad lista pos_x (+ pos_y 1) image (+ contador 1) elemento reemplazo)
                            (rellenar_profundidad lista (+ pos_x 1) 0 image contador elemento reemplazo))]

                     [(<= pos_y (largo_pos_x image))
                        (if (null? lista)
                          null
                         (if (= (d_bit (encontrar_pixel lista pos_x pos_y)) elemento)
                            (cons (encontrar_pixel lista pos_x pos_y) (rellenar_profundidad lista pos_x (+ pos_y 1) image (+ contador 1) elemento reemplazo))
                            (cons (cambiar_b_bit (encontrar_pixel lista pos_x pos_y) reemplazo) (rellenar_profundidad lista pos_x (+ pos_y 1) image (+ contador 1) elemento reemplazo))))]

                     [else (rellenar_profundidad lista (+ pos_x 1) 0 image contador elemento reemplazo)]))))

                                 
      ; Función que filtra los elementos que tienen la misma profundidad e para bitmap y hexmap
      (define filtro_profundidad (lambda (formato_pixeles e)
         (if (null? formato_pixeles)
              null
              (if (or (null? (car formato_pixeles)) (= (d_bit (car formato_pixeles)) e))
                  (filtro_profundidad (cdr formato_pixeles) e)
                  (cons (car formato_pixeles) (filtro_profundidad (cdr formato_pixeles) e))))))
                      
     (if (null? lista)
         null
         (if (= (length (pixel_formato image_ingresada)) 1)
             (cons image_ingresada null)
             (cons (modificar_formato_image image_ingresada (rellenar_profundidad (pixel_formato image_ingresada) 0 0 image_ingresada 0 (d_bit (car lista)) reemplazo))
             (profundidad_bit_hex image_ingresada (filtro_profundidad lista (d_bit (car lista))) reemplazo))))))


    ; Función que crea la lista de imagenes de profundidad con pixmap
    (define profundidad_rgb (lambda (image_ingresada lista)

     ; Función que rellenar pixeles restantes en blanco (255, 255, 255)
     (define rellenar_profundidad (lambda (lista pos_x pos_y image contador elemento)
          (if (or (null? lista) (= contador (* (ancho_image image) (largo_image image))))
              null
             (cond

                   [(null? (encontrar_pixel lista pos_x pos_y))
                       (if (<= pos_y (largo_pos_x image))
                         (rellenar_profundidad lista pos_x (+ pos_y 1) image (+ contador 1) elemento)
                         (rellenar_profundidad lista (+ pos_x 1) 0 image contador elemento))]

                    [(<= pos_y (largo_pos_x image))
                      (if (= (getD (encontrar_pixel lista pos_x pos_y)) elemento)
                        (cons (encontrar_pixel lista pos_x pos_y) (rellenar_profundidad lista pos_x (+ pos_y 1) image (+ contador 1) elemento))
                        (cons (setB (setG (setR (encontrar_pixel lista pos_x pos_y) 255) 255) 255) (rellenar_profundidad lista pos_x (+ pos_y 1) image (+ contador 1) elemento)))]

                    [else (rellenar_profundidad lista (+ pos_x 1) 0 image contador elemento)]))))

      ; Función filtra los elementos que tienen la misma profundidad e para pixmap
      (define filtro_profundidad (lambda (formato_pixeles e)
         (if (null? formato_pixeles)
              null
              (if (or (null? (car formato_pixeles)) (= (getD (car formato_pixeles)) e))
                  (filtro_profundidad (cdr formato_pixeles) e)
                  (cons (car formato_pixeles) (filtro_profundidad (cdr formato_pixeles) e))))))

      (if (null? lista)
         null
        (if (= (length (pixel_formato image_ingresada)) 1)
            (cons image_ingresada null)
            (cons (modificar_formato_image image_ingresada (rellenar_profundidad (pixel_formato image_ingresada) 0 0 image_ingresada 0 (getD (car lista))))
              (profundidad_rgb image_ingresada (filtro_profundidad lista (getD (car lista)))))))))

                      
     (cond
       [(bitmap? image_ingresada) (profundidad_bit_hex image_ingresada (pixel_formato image_ingresada) 0)]
       [(hexmap? image_ingresada) (profundidad_bit_hex image_ingresada (pixel_formato image_ingresada) "#000000")]
       [(pixmap? image_ingresada) (profundidad_rgb image_ingresada (pixel_formato image_ingresada))]
       [else image_ingresada])))


; exportar la funcion al exterior
#|
(provide image)
(provide bitmap?)
(provide hexmap?)
(provide pixmap?)
(provide compressed?)
(provide flipH)
(provide flipV)
(provide crop)
(provide imgRGB->imgHex)
(provide histogram)
(provide rotate90)
(provide compress)
(provide edit)
(provide image->string)
(provide decompress)
|#
(provide (all-defined-out))
