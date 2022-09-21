#lang racket

#|
-----------------------------------TDA PIXRGB-D -----------------------------------------------------------------

----------------------------------REPRESENTACION-------------------------------------------------------------

 El TDA representa pixel tipo pixmap como una lista con seis parametros, un entero para la coordenada x y otro
 para la coordenada y, tres enteros que representen el color rojo, verde y azul respectivamente y un último
 entero para guardar la profundidad
 (int X int X int X int X int X int)

|#
;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Dominio: x (int) X y (int) X c1 (int) X c2 (int) X c3 (int) X d (int)
; Recorrido: pixrgb-d
; Descripción: Función constructora de un un pixel pixmap
(define pixrgb-d (lambda (x y c1 c2 c3 d) (list x y c1 c2 c3 d)))

; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Dominio: x (int) X y (int) X c1 (int) X c2 (int) X c3 (int)
; Recorrido: boleano
; Descripción: Función que verifica si el argumento es un pixrgb-d
(define pixrgb-d? (lambda (pixrgb-d)
  (if (and (= (length pixrgb-d) 6) (number? (x_rgb pixrgb-d)) (number? (y_rgb pixrgb-d))
           (number? (getR pixrgb-d)) (>= (getR pixrgb-d) 0) (<= (getR pixrgb-d) 255) 
           (number? (getG pixrgb-d)) (>= (getG pixrgb-d) 0) (<= (getG pixrgb-d) 255)
           (number? (getB pixrgb-d)) (>= (getB pixrgb-d) 0) (<= (getB pixrgb-d) 255)
           (number? (getD pixrgb-d)))
      #t #f)))

; Dominio: x (int) X y (int) X c1 (int) X c2 (int) X c3 (int)
; Recorrido: Boleano
; Descripción: Función que verifica si el argumento es un pixrgb-d comprimido
(define pixrgb-d_compressed? (lambda (pixrgb-d)
  (if (and (= (length pixrgb-d) 6) (number? (x_rgb pixrgb-d)) (number? (y_rgb pixrgb-d)) (compress_rgb? pixrgb-d) (number? (getD pixrgb-d)))
      #t #f)))

; Dominio: pixrgb-d, lista de color (list)
; Recorrido: Boleano
; Descripción: Función que compara si un pixrgb-d tiene colores iguales a la lista de colores entregado
(define color_igual (lambda (pixel_1 color_lista)
         (if (and (= (getR pixel_1) (car color_lista)) (= (getG pixel_1) (cadr color_lista)) (= (getB pixel_1) (caddr color_lista)))
             #t #f)))

; Dominio: pixrgb-d
; Recorrido: boleano
; Descripción: Función que verifica si se comprimio un pixrgb-d
(define compress_rgb? (lambda (pixrgb-d)
     (if (and (string? (getR pixrgb-d)) (string? (getG pixrgb-d)) (string? (getB pixrgb-d))) #t #f)))



;----------------------------------- SELECTORES--------------------------------------------------------------

; Dominio: pixrgb-d
; Recorrido: posición x (int)
; Descripción: Función para recuperar la posición x de un pixrgb-d
(define x_rgb (lambda (pixrgb-d) (car pixrgb-d)))

; Dominio: pixrgb-d
; Recorrido: posición y (int)
; Descripción: Función para recuperar la posición y de un pixrgb-d
(define y_rgb (lambda (pixrgb-d) (cadr pixrgb-d)))

; Dominio: pixrgb-d
; Recorrido: c1 (int)
; Descripción: Función para recuperar el color rojo de un pixrgb-d
(define getR (lambda (pixrgb-d) (caddr pixrgb-d)))

; Dominio: pixrgb-d
; Recorrido: c2 (int)
; Descripción: Función para recuperar el color verde de un pixrgb-d
(define getG (lambda (pixrgb-d) (cadddr pixrgb-d)))

; Dominio: pixrgb-d
; Recorrido: c3 (int)
; Descripción: Función para recuperar el color azul de un pixrgb-d
(define getB (lambda (pixrgb-d) (list-ref pixrgb-d 4)))

; Dominio: pixrgb-d
; Recorrido: d (int)
; Descripción: Función para recuperar la profundidad de un pixrgb-d
(define getD (lambda (pixrgb-d) (list-ref pixrgb-d 5)))

; Dominio: pixrgb-d
; Recorrido: colores RGB (list)
; Descripción: función que recopila los colores en lista
(define color_lista (lambda (pixel) (list (getR pixel) (getG pixel) (getB pixel))))


; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica la posicion x de un pixrgb-d
(define cambiar_x_rgb (lambda (pixrgb-d_pasado x_nuevo)               
      (pixrgb-d x_nuevo (y_rgb pixrgb-d_pasado) (getR pixrgb-d_pasado) (getG pixrgb-d_pasado) (getB pixrgb-d_pasado) (getD pixrgb-d_pasado))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica la posicion y de un pixrgb-d
(define cambiar_y_rgb (lambda (pixrgb-d_pasado y_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) y_nuevo (getR pixrgb-d_pasado) (getG pixrgb-d_pasado) (getB pixrgb-d_pasado) (getD pixrgb-d_pasado))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica el color rojo de un pixrgb-d
(define setR (lambda (pixrgb-d_pasado c1_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) c1_nuevo (getG pixrgb-d_pasado) (getB pixrgb-d_pasado) (getD pixrgb-d_pasado))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica el color verde de un pixrgb-d
(define setG (lambda (pixrgb-d_pasado c2_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) (getR pixrgb-d_pasado) c2_nuevo (getB pixrgb-d_pasado) (getD pixrgb-d_pasado))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica el color azul de un pixrgb-d
(define setB (lambda (pixrgb-d_pasado c3_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) (getR pixrgb-d_pasado) (getG pixrgb-d_pasado) c3_nuevo (getD pixrgb-d_pasado))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica la profundidad de un pixrgb-d
(define setD (lambda (pixrgb-d_pasado d_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) (getR pixrgb-d_pasado) (getG pixrgb-d_pasado)(getB pixrgb-d_pasado) d_nuevo)))

; Dominio: int
; Recorrido: int
; Descripción: Función que aumenta el canal en uno
(define incCh (lambda (entero) (+ entero 1)))

; Dominio: función_selectora X función_modificadora X función_operación X pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que modifica un canal de colores o profundidad
(define adjustChannel (lambda (funcion_get funcion_set funcion_operacion)
                      (lambda (pixrgb-d_pasado) (funcion_set pixrgb-d_pasado (funcion_operacion (funcion_get pixrgb-d_pasado))))))

;------------------------------------------- OTRAS FUNCIONES ----------------------------------------------

; Dominio: formato de pixeles (list)
; Recorrido: list
; Descripción: función que recopila la cantidad de elemento de cada tipo de una lista
; Tipo de recursión: Natural
(define histograma_rgb (lambda (formato_pixeles)

    ; Descripción: Función que recupera la lista por los elementos iguales a e
    ; Tipo de recursión: Natural
    (define filtro_iguales_rgb (lambda (formato_pixeles e)
        (if (null? formato_pixeles)
            null
            (if (not (color_igual (car formato_pixeles) e))
                (cons (car formato_pixeles) (filtro_iguales_rgb (cdr formato_pixeles) e))
                (filtro_iguales_rgb (cdr formato_pixeles) e)))))
  
    ; Descripción: Función que cuenta los elementos iguales a e
    ; Tipo de recursión: Cola
    (define rgb_iguales (lambda (formato_pixeles e result)
        (if (null? formato_pixeles)
           result
           (if (color_igual (car formato_pixeles) e)
               (rgb_iguales (cdr formato_pixeles) e (+ result 1))
               (rgb_iguales (cdr formato_pixeles) e result)))))

                         
    (if (null? formato_pixeles)
        null
        (if (color_igual (car formato_pixeles) (list -1 -1 -1))
            (histograma_rgb (filtro_iguales_rgb formato_pixeles (color_lista (car formato_pixeles))))
            (cons (list (rgb_iguales formato_pixeles (color_lista (car formato_pixeles)) 0) (color_lista (car formato_pixeles))) (histograma_rgb (filtro_iguales_rgb formato_pixeles (color_lista (car formato_pixeles)))))))))

; Dominio: histograma de rgb
; Rec: list
; Descripción: Función que obtiene el rgb más repetido del histograma
; Tipo de recursión: Cola
(define rgb_mayor (lambda (lista_rgb result)
    (if (null? lista_rgb)
        (car (cdr result))
        (if (> (car(car lista_rgb)) (car result))
            (rgb_mayor (cdr lista_rgb) (car lista_rgb))
            (rgb_mayor (cdr lista_rgb) result)))))


; Dominio: formato de pixeles (list) X elemento (list)
; Recorrido: list
; Descripción: Función que crea una lista sin el rgb más repetido
; Tipo de recursión: Natural
(define compress-formato-rgb (lambda (lista elemento)

    ; Descripción: Función que convierte los colores de rgb de enteros a string
    (define convertir_rgb_hex_pixel (lambda (pixel)

        ; Función para convertir un numero a hex
        (define valor_hex (lambda (a)
           (cond
              [(= a 1) "1"][(= a 2) "2"][(= a 3) "3"][(= a 4) "4"]
              [(= a 5) "5"][(= a 6) "6"][(= a 7) "7"][(= a 8) "8"]
              [(= a 9) "9"][(= a 10) "A"][(= a 11) "B"][(= a 12) "C"]
              [(= a 13) "D"][(= a 14) "E"][(= a 15) "F"][else "0"])))

         ; Función que transforma un color a string
        (define rgb->hex (lambda (a)
           (string-append (valor_hex (quotient a 16)) (valor_hex (remainder a 16)))))

       (setB (setG (setR pixel (rgb->hex (getR pixel))) (rgb->hex (getG pixel))) (rgb->hex (getB pixel)))))
                               
    (if (null? lista)
        null
        (if (color_igual (car lista) elemento)
            (cons (convertir_rgb_hex_pixel (car lista))(compress-formato-rgb (cdr lista) elemento))
            (cons (car lista) (compress-formato-rgb (cdr lista) elemento))))))


; Dominio: formaro de pixeles (list)
; Recorrido: list
; Descripción: Función que descomprime cada pixrgb-d comprimido
; Tipo de recursión: Natural
(define descompress-formato-rgb (lambda (lista)

     ; Descripción: Función que convierte una pixrgb-d comprimido a su forma original
    (define convertir_valor_hex_rgb (lambda (pixel)

        ; Función para convertir un caracter a numero
        (define valor_entero (lambda (a)
          (cond
             [(char=? a #\1) 1][(char=? a #\2) 2][(char=? a #\3) 3][(char=? a #\4) 4]
             [(char=? a #\5) 5][(char=? a #\6) 6][(char=? a #\7) 7][(char=? a #\8) 8]
             [(char=? a #\9) 9][(char=? a #\A) 10][(char=? a #\B) 11][(char=? a #\C) 12]
             [(char=? a #\D) 13][(char=? a #\E) 14][(char=? a #\F) 15][else 0])))
                                      
     ; Función que recupera los valores de cada string
    (define obtener_valor (lambda (string_ingresado posicion)
        (if (> posicion (- (string-length string_ingresado) 1))
            null
            (cons (valor_entero (string-ref string_ingresado posicion)) (obtener_valor string_ingresado (+ posicion 1))))))

    ; Función que transforma una lista de valores en un entero
    (define hex->rgb (lambda (lista largo result)
         (if (< largo 0)
             result
             (hex->rgb (cdr lista) (- largo 1) (+ result (* (car lista) (expt 16 largo)))))))

    (setB (setG (setR pixel (hex->rgb (obtener_valor (getR pixel) 0) 1 0)) (hex->rgb (obtener_valor (getG pixel) 0) 1 0)) (hex->rgb (obtener_valor (getB pixel) 0) 1 0))))

                                  
     (if (null? lista)
         null
         (if (compress_rgb? (car lista))
             (cons (convertir_valor_hex_rgb (car lista)) (descompress-formato-rgb (cdr lista)))
             (cons (car lista) (descompress-formato-rgb (cdr lista)))))))

; Dominio: pixrgb-d
; Recorrido: pixrgb-d
; Descripción: Función que invierte el color de un pixrgb-d, invertColorRGB
(define invertColorRGB (lambda (pixrgb-d_pasado)
     (define invertir_color (lambda (color)
           (abs (- color 255))))

     (if (pixrgb-d? pixrgb-d_pasado)
        (setB (setG (setR pixrgb-d_pasado (invertir_color (getR pixrgb-d_pasado))) (invertir_color (getG pixrgb-d_pasado))) (invertir_color (getB pixrgb-d_pasado)))
        pixrgb-d_pasado)))

; Dominio: formato de pixeles (list) X largo (int)
; Recorrido: string
; Descripción: Función que convierte el formato de pixeles en una cadena de string, pixrgb->string
; Tipo de recursión: Natural
(define pixrgb->string (lambda (formato_image largo)

    ; Función para convertir un numero a hex
    (define valor_hex (lambda (a)
       (cond
          [(= a 1) "1"][(= a 2) "2"][(= a 3) "3"][(= a 4) "4"]
          [(= a 5) "5"][(= a 6) "6"][(= a 7) "7"][(= a 8) "8"]
          [(= a 9) "9"][(= a 10) "A"][(= a 11) "B"][(= a 12) "C"]
          [(= a 13) "D"][(= a 14) "E"][(= a 15) "F"][else "0"])))

    ; Función que transforma un c1 a hex
    (define rgb->hex (lambda (a)
       (string-append (valor_hex (quotient a 16)) (valor_hex (remainder a 16)))))                 

    ; Función que tranforma un color rgb->hex
    (define convertir_rgb (lambda (c1 c2 c3)
        (string-append (rgb->hex c1)(rgb->hex c2)(rgb->hex c3))))
                                             
    (define fila_rgb (lambda (formato_image fila)
        (if (null? formato_image)
            "\n"
            (if (null? (car formato_image))
                (string-append "       " (fila_rgb (cdr formato_image) fila))
                (if (= (x_rgb (car formato_image)) fila)
                      (string-append "#" (convertir_rgb (getR (car formato_image)) (getG (car formato_image)) (getB (car formato_image))) " " (fila_rgb (cdr formato_image) fila))
                       (fila_rgb (cdr formato_image) fila))))))

     (define formar_string (lambda (formato_image largo fila)
          (if (<= fila largo)
              (string-append (fila_rgb formato_image fila) (formar_string formato_image largo (+ fila 1)))
              "\n")))
                         
    (formar_string formato_image largo 0)))

; exportar la funcion al exterior
(provide (all-defined-out))














