#lang racket



;-----------------------------------TDA PIXRGB-D -----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; El TDA representa pixel tipo rgb con parametros (posicion_x, posicion_y, color_rojo, color_verde, color_azul, profundidad).
; Se representa un pixel como una lista de seis parametros
; (posicionX, posicionY, colorR, colorV, colorA)

;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Descripción: formato de un pixel
; Dom: x int X y int X c1 int X c2 int X c3 int X d int
; Rec: pixrgb-d
(define pixrgb-d (lambda (x y c1 c2 c3 d)
               (list x y c1 c2 c3 d)))


;----------------------------------- SELECTORES--------------------------------------------------------------

; Descripción: obtener la posición x de un pixrgb-d
; Dom: pixrgb-d
; Rec: posición x (int)
(define x_rgb (lambda (pixrgb-d) (car pixrgb-d)))

; Descripción: obtener la posicion y de un pixrgb-d
; Dom: pixrgb-d
; Rec: posición y (int)
(define y_rgb (lambda (pixrgb-d) (cadr pixrgb-d)))

; Descripción: obtener el color rojo de un pixrgb-d
; Dom: pixrgb-d
; Rec: c1 (int)
(define c1_rgb (lambda (pixrgb-d) (caddr pixrgb-d)))

; Descripción: obtener el color verde de un pixrgb-d
; Dom: pixrgb-d
; Rec: c2 (int)
(define c2_rgb (lambda (pixrgb-d) (cadddr pixrgb-d)))

; Descripción: obtener el color azul de un pixrgb-d
; Dom: pixrgb-d
; Rec: c3 (int)
(define c3_rgb (lambda (pixrgb-d) (list-ref pixrgb-d 4)))

; Descripción: obtener la profundidad de un pixrgb-d
; Dom: pixrgb-d
; Rec: d (int)
(define d_rgb (lambda (pixrgb-d) (list-ref pixrgb-d 5)))


; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Descripción: función que verifica si el argumento es un pixrgb-d
; Dom: x (int) X y (int) X c1 (int) X c2 (int) X c3 (int)
; Rec: Boleano
(define pixrgb-d? (lambda (pixrgb-d)
  (if (and (= (length pixrgb-d) 6)
           (number? (x_rgb pixrgb-d))
           (number? (y_rgb pixrgb-d))
           (number? (c1_rgb pixrgb-d)) (>= (c1_rgb pixrgb-d) 0) (<= (c1_rgb pixrgb-d) 255) 
           (number? (c2_rgb pixrgb-d)) (>= (c2_rgb pixrgb-d) 0) (<= (c2_rgb pixrgb-d) 255)
           (number? (c3_rgb pixrgb-d)) (>= (c3_rgb pixrgb-d) 0) (<= (c3_rgb pixrgb-d) 255)
           (number? (d_rgb pixrgb-d))

           ) #t #f)))


; Descripción: función que verifica si el argumento es un pixrgb-d comprimido
; Dom: x (int) X y (int) X c1 (int) X c2 (int) X c3 (int)
; Rec: Boleano
(define pixrgb-d_compressed? (lambda (pixrgb-d)
  (if (and (= (length pixrgb-d) 6)
           (number? (x_rgb pixrgb-d))
           (number? (y_rgb pixrgb-d))
           (compress_rgb? pixrgb-d)
           (number? (d_rgb pixrgb-d))

           ) #t #f)))

; Descripción: función que compara si dos pixrgb-d tienen colores iguales
; Dom: pixrgb-d, lista color
; Rec: Boleano
(define color_igual (lambda (pixel_1 color_lista)
         (if (and
              (= (c1_rgb pixel_1) (car color_lista))
              (= (c2_rgb pixel_1) (cadr color_lista))
              (= (c3_rgb pixel_1) (caddr color_lista))) #t #f)))


; Descripción: función que verifica si se comprimio un pixrgb-d
; Dom: pixrgb-d
; Rec: boleano
(define compress_rgb? (lambda (pixrgb-d)
     (if (and (string? (c1_rgb pixrgb-d))
              (string? (c2_rgb pixrgb-d))
              (string? (c3_rgb pixrgb-d))) #t #f)))

; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Descripción: modificar la posicion x de un pixrgb-d
; Dom: pixrgb-d
; Rec: pixrgb-d
(define cambiar_x_rgb (lambda (pixrgb-d_pasado x_nuevo)               
      (pixrgb-d x_nuevo (y_rgb pixrgb-d_pasado) (c1_rgb pixrgb-d_pasado) (c2_rgb pixrgb-d_pasado) (c3_rgb pixrgb-d_pasado) (d_rgb pixrgb-d_pasado))))


; Descripción: modificar la posicion y de un pixrgb-d
; Dom: pixrgb-d
; Rec: pixrgb-d
(define cambiar_y_rgb (lambda (pixrgb-d_pasado y_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) y_nuevo (c1_rgb pixrgb-d_pasado) (c2_rgb pixrgb-d_pasado) (c3_rgb pixrgb-d_pasado) (d_rgb pixrgb-d_pasado))))


; Descripción: modificar el color rojo de un pixrgb-d
; Dom: pixrgb-d
; Rec: pixrgb-d
(define cambiar_c1_rgb (lambda (pixrgb-d_pasado c1_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) c1_nuevo (c2_rgb pixrgb-d_pasado) (c3_rgb pixrgb-d_pasado) (d_rgb pixrgb-d_pasado))))

; Descripción: modificar el color verde de un pixrgb-d
; Dom: pixrgb-d
; Rec: pixrgb-d
(define cambiar_c2_rgb (lambda (pixrgb-d_pasado c2_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) (c1_rgb pixrgb-d_pasado) c2_nuevo (c3_rgb pixrgb-d_pasado) (d_rgb pixrgb-d_pasado))))

; Descripción: modificar el color azul de un pixrgb-d
; Dom: pixrgb-d
; Rec: pixrgb-d
(define cambiar_c3_rgb (lambda (pixrgb-d_pasado c3_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) (c1_rgb pixrgb-d_pasado) (c2_rgb pixrgb-d_pasado) c3_nuevo (d_rgb pixrgb-d_pasado))))

; Descripción: modificar la profundidad de un pixrgb-d
; Dom: pixrgb-d
; Rec: pixrgb-d
(define cambiar_d_rgb (lambda (pixrgb-d_pasado d_nuevo)               
      (pixrgb-d (x_rgb pixrgb-d_pasado) (y_rgb pixrgb-d_pasado) (c1_rgb pixrgb-d_pasado) (c2_rgb pixrgb-d_pasado)(c3_rgb pixrgb-d_pasado) d_nuevo)))

;-------------------------------------------------- SELECTORES--------------------------------------------------------------------------

; Descripción: función que recopila los colores en lista
; Dom: pixrgb-d
; Rec: lista c1, c2 y c3
(define color_lista (lambda (pixel)
         (list (c1_rgb pixel) (c2_rgb pixel) (c3_rgb pixel))))

; Descripción: función que filtra la lista por los elementos iguales a e
; Dom: lista x elemento
; Rec: lista
; tipo de recursión: Natural
(define filtro_iguales_rgb (lambda (formato_image e)
    (if (null? formato_image)
        null
        (if (not (color_igual (car formato_image) e))
            (cons (car formato_image) (filtro_iguales_rgb (cdr formato_image) e))
            (filtro_iguales_rgb (cdr formato_image) e)))))


; Descripción: función que cuenta los elementos iguales a e en una lista
; Dom: lista (pixeles) x elemento
; Rec: lista
; tipo de recursión: cola
(define rgb_iguales (lambda (formato_image e result)
       (if (null? formato_image)
           result
           (if (color_igual (car formato_image) e)
               (rgb_iguales (cdr formato_image) e (+ result 1))
               (rgb_iguales (cdr formato_image) e result)))))

;--------------------------------------------------- OTROS ----------------------------------------------

; Descripción: función que recopila la cantidad de elemento de cada tipo de una lista
; Dom: lista (pixeles)
; Rec: lista
(define histograma_rgb (lambda (formato_image)
    (if (null? formato_image)
        null
        (if (color_igual (car formato_image) (list -1 -1 -1))
            (histograma_rgb (filtro_iguales_rgb formato_image (color_lista (car formato_image))))
            (cons (list (rgb_iguales formato_image (color_lista (car formato_image)) 0) (color_lista (car formato_image))) (histograma_rgb (filtro_iguales_rgb formato_image (color_lista (car formato_image)))))))))

; Descripción: función que obtiene el rgb más repetido del histograma
; Dom: lista del histograma
; Rec: lista
(define rgb_mayor (lambda (lista_rgb result)
    (if (null? lista_rgb)
        (car (cdr result))
        (if (> (car(car lista_rgb)) (car result))
            (rgb_mayor (cdr lista_rgb) (car lista_rgb))
            (rgb_mayor (cdr lista_rgb) result)))))


(define convertir_rgb_hex_pixel (lambda (pixel)

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

     (cambiar_c3_rgb (cambiar_c2_rgb (cambiar_c1_rgb pixel (rgb->hex (c1_rgb pixel))) (rgb->hex (c2_rgb pixel))) (rgb->hex (c3_rgb pixel)))

                                  ))


; Descripción: función que crea una lista sin el rgb más repetido
(define compress-formato-rgb (lambda (lista elemento)
    (if (null? lista)
        null
        (if (color_igual (car lista) elemento)
            (cons (convertir_rgb_hex_pixel (car lista))(compress-formato-rgb (cdr lista) elemento))
            (cons (car lista) (compress-formato-rgb (cdr lista) elemento))))))


(define convertir_valor_hex_rgb (lambda (pixel)

    ; función auxiliar para convertir un caracter a numero
     (define valor_entero (lambda (a)
       (cond
          [(char=? a #\1) 1][(char=? a #\2) 2][(char=? a #\3) 3][(char=? a #\4) 4]
          [(char=? a #\5) 5][(char=? a #\6) 6][(char=? a #\7) 7][(char=? a #\8) 8]
          [(char=? a #\9) 9][(char=? a #\A) 10][(char=? a #\B) 11][(char=? a #\C) 12]
          [(char=? a #\D) 13][(char=? a #\E) 14][(char=? a #\F) 15][else 0])))


    ; función que recupera los valores de cada string
    (define obtener_valor (lambda (string_ingresado posicion)
        (if (> posicion (- (string-length string_ingresado) 1))
            null
            (cons (valor_entero (string-ref string_ingresado posicion)) (obtener_valor string_ingresado (+ posicion 1)))
         )))

    ; función auxiliar que transforma una lista de valores en un entero
    (define hex->rgb (lambda (lista largo result)
         (if (< largo 0)
             result
             (hex->rgb (cdr lista) (- largo 1) (+ result (* (car lista) (expt 16 largo)))))))


     (cambiar_c3_rgb
      (cambiar_c2_rgb
       (cambiar_c1_rgb pixel
          (hex->rgb (obtener_valor (c1_rgb pixel) 0) 1 0))
            (hex->rgb (obtener_valor (c2_rgb pixel) 0) 1 0))
              (hex->rgb (obtener_valor (c3_rgb pixel) 0) 1 0))

             ))


(define descompress-formato-rgb (lambda (lista)
     (if (null? lista)
         null
         (if (compress_rgb? (car lista))
             (cons (convertir_valor_hex_rgb (car lista)) (descompress-formato-rgb (cdr lista)))
             (cons (car lista) (descompress-formato-rgb (cdr lista))))

                                  )))

(define ola (list (list 0 0 "0A" "0A" "0A" 10)
   (list 0 1 20 20 20 20)
   (list 0 2 30 30 30 30)
   (list 1 0 40 40 40 40)
   (list 1 1 50 50 50 50)
   (list 1 2 60 60 60 60)))

; Descripción: invertColorRGB
; Dom: pixrgb-d
; Rec: pixrgb-d
(define invertColorRGB (lambda (pixrgb-d_pasado)
     (define invertir_color (lambda (color)
           (abs (- color 255))))

     (if (pixrgb-d? pixrgb-d_pasado)

         (pixrgb-d (x_rgb pixrgb-d_pasado)
               (y_rgb pixrgb-d_pasado)
               (invertir_color (c1_rgb pixrgb-d_pasado))
               (invertir_color (c2_rgb pixrgb-d_pasado))
               (invertir_color (c3_rgb pixrgb-d_pasado))
               (d_rgb pixrgb-d_pasado))
         
         pixrgb-d_pasado)))



; Descripción: pixrgb->string
; Dom: formato image x largo image
; Rec: string
(define pixrgb->string (lambda (formato_image largo)


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
                         
                            
    (define fila_rgb (lambda (formato_image fila)
        (if (null? formato_image)
            "\n"
            (if (= (x_rgb (car formato_image)) fila)
                (string-append (convertir_rgb (c1_rgb (car formato_image)) (c2_rgb (car formato_image)) (c3_rgb (car formato_image))) " " (fila_rgb (cdr formato_image) fila))
                (fila_rgb (cdr formato_image) fila)))))

     (define formar_string (lambda (formato_image largo fila)
          (if (<= fila largo)
              (string-append (fila_rgb formato_image fila) (formar_string formato_image largo (+ fila 1)))
              "\n")))
                         
    (formar_string formato_image largo 0)))



; exportar la funcion al exterior
(provide (all-defined-out))




