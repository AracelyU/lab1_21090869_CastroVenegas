#lang racket

#|
-----------------------------------TDA PIXHEX-D -----------------------------------------------------------------

----------------------------------REPRESENTACION-------------------------------------------------------------

 El TDA representa pixel tipo hexmap como una lista de 4 parametros, uno para la posicion en x, otro para la
 posicion en y, un string para el color hexadecimal y el último para la profundidad
(int X int X string X int)

|#
;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Dominio: x (int) X y (int) X hex (int) X d (int)
; Recorrido: pixhex-d
; Descripción: Función constructora de un pixhex-d
(define pixhex-d (lambda (x y hex d) (list x y hex d)))

; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Dominio: x (int) X y (int) X hex (string) X d (int)
; Recorrido: boleano
; Descripción: Función que verifica si el argumento es un pixhex-d
(define pixhex-d? (lambda (pixhex-d)
    (if (and (= (length pixhex-d) 4)(number? (x_hex pixhex-d))(number? (y_hex pixhex-d))(string? (hex pixhex-d))(number? (d_hex pixhex-d)))
        #t #f)))

; Dominio: x (int) X y (int) X hex (string) X d (int)
; Recorrido: boleano
; Descripción: Función que verifica si el argumento es un pixhex-d esta comprimida
(define pixhex-d_compressed? (lambda (pixhex-d)
    (if (and (= (length pixhex-d) 4) (number? (x_hex pixhex-d)) (number? (y_hex pixhex-d)) (list? (hex pixhex-d))(number? (d_hex pixhex-d)))
        #t #f)))

; Dominio: pixhex-d
; Recorrido: boleano
; Descripción: Función que verifica si se comprimio un pixhex-d
(define compress_hex? (lambda (pixhex-d)
     (if (string? (hex pixhex-d)) #t #f)))

;----------------------------------- SELECTORES--------------------------------------------------------------

; Dominio: pixhex-d
; Recorrido: posición x (int)
; Descripción: Función para recuperar la posición x de un pixhex-d
(define x_hex (lambda (pixhex-d) (car pixhex-d)))

; Dominio: pixhex-d
; Recorrido: posición y (int)
; Descripción: Función para recuperar la posición y de un pixhex-d
(define y_hex (lambda (pixhex-d) (cadr pixhex-d)))

; Dominio: pixhex-d
; Recorrido: hex (string)
; Descripción: Función para recuperar el string un pixhex-d
(define hex (lambda (pixhex-d) (caddr pixhex-d)))

; Dominio: pixhex-d
; Recorrido: profundidad (int)
; Descripción: Función para recuperar la profundidad de un pixhex-d
(define d_hex (lambda (pixhex-d) (cadddr pixhex-d)))


; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Dominio: pixhex-d
; Recorrido: pixhex-d
; Descripción: Función que modifica la posicion x de un pixhex-d
(define cambiar_x_hex (lambda (pixhex-d_pasado x_nuevo)               
      (pixhex-d x_nuevo (y_hex pixhex-d_pasado) (hex pixhex-d_pasado) (d_hex pixhex-d_pasado))))

; Dominio: pixhex-d
; Recorrido: pixhex-d
; Descripción: Función que modifica la posicion y de un pixhex-d
(define cambiar_y_hex (lambda (pixhex-d_pasado y_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) y_nuevo (hex pixhex-d_pasado) (d_hex pixhex-d_pasado))))

; Dominio: pixhex-d
; Recorrido: pixhex-d
; Descripción: Función que modifica el string de un pixhex-d
(define cambiar_h_hex (lambda (pixhex-d_pasado h_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) (y_hex pixhex-d_pasado) h_nuevo (d_hex pixhex-d_pasado))))

; Dominio: pixhex-d
; Recorrido: pixhex-d
; Descripción: Función que modifica la profundidad de un pixhex-d
(define cambiar_d_hex (lambda (pixhex-d_pasado d_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) (y_hex pixhex-d_pasado) (hex pixhex-d_pasado) d_nuevo)))


;--------------------------------------- OTRAS FUNCIONES -----------------------------------------------------------------------------------

; Dominio: formato de pixeles (list)
; Recorrido: list
; Descripción: Función que recopila la cantidad de elemento de cada tipo de una lista
; Tipo de recursión: Utiliza recursión natural y cola
(define histograma_hex (lambda (formato_pixeles)

   ; Descripción: Función que cuenta los elementos iguales a e en una lista, recursión de cola
   (define hex_iguales (lambda (formato_pixeles e result)
       (if (null? formato_pixeles)
           result
           (if (and (string-ci=? (hex (car formato_pixeles)) e) )
               (hex_iguales (cdr formato_pixeles) e (+ result 1))
               (hex_iguales (cdr formato_pixeles) e result)))))

   ; Descripción: Función que recupera los elementos iguales a e, recursión natural
   (define filtro_iguales_hex (lambda (formato_pixeles e)
    (if (null? formato_pixeles)
        null
        (if (not (string-ci=? (hex (car formato_pixeles)) e))
            (cons (car formato_pixeles) (filtro_iguales_hex (cdr formato_pixeles) e))
            (filtro_iguales_hex (cdr formato_pixeles) e)))))
                         
    (if (null? formato_pixeles)
        null
        (cons (list (hex_iguales formato_pixeles (hex (car formato_pixeles)) 0)(hex (car formato_pixeles)))
              (histograma_hex (filtro_iguales_hex formato_pixeles (hex (car formato_pixeles))))))))


; Dominio: formato de pixeles (list) X elemento (string)
; Recorrido: list
; Descripción: función que crea una lista sin el hex más repetido
; Tipo de recursión: Natural
(define compress-formato-hex (lambda (lista elemento)
    ; Descripción: Función que cambia un hexmap a pixmap
   (define convertir_hex_rgb (lambda (string_ingresado)

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

       (list
           (hex->rgb (obtener_valor (substring string_ingresado 0 2) 0) 1 0)
           (hex->rgb (obtener_valor (substring string_ingresado 2 4) 0) 1 0)
           (hex->rgb (obtener_valor (substring string_ingresado 4 6) 0) 1 0))))

                               
     (if (null? lista)
         null
         (if (string-ci=? (hex (car lista)) elemento)
             (cons (cambiar_h_hex (car lista) (convertir_hex_rgb (hex (car lista)))) (compress-formato-hex (cdr lista) elemento))
             (cons (car lista) (compress-formato-hex (cdr lista) elemento))))))


; Dominio: formato de pixeles (list)
; Recorrido: formato de pixeles (list)
; Descripción: Función que devuelve los valores perdidos tras compress
; Tipo de recursión: Natural
(define descompress-formato-hex (lambda (lista)

   ; Descripción: Función que cambia una lista de colores pixmap a un string hexmap, para descompress
   (define convertir_rgb_hex_lista (lambda (lista)
                            
       ; Función para convertir un numero a hex
       (define valor_hex (lambda (a)
          (cond
             [(= a 1) "1"][(= a 2) "2"][(= a 3) "3"][(= a 4) "4"]
             [(= a 5) "5"][(= a 6) "6"][(= a 7) "7"][(= a 8) "8"]
             [(= a 9) "9"][(= a 10) "A"][(= a 11) "B"][(= a 12) "C"]
             [(= a 13) "D"][(= a 14) "E"][(= a 15) "F"][else "0"])))

       ; Función que transforma un color pixmap a hexmap
       (define rgb->hex (lambda (a)
          (string-append (valor_hex (quotient a 16)) (valor_hex (remainder a 16)))))                 

      (string-append (rgb->hex (car lista)) (rgb->hex (cadr lista)) (rgb->hex (caddr lista)))))
                          
    (if (null? lista)
        null
        (if (compress_hex? (car lista))
            (cons (car lista) (descompress-formato-hex (cdr lista)))
            (cons (cambiar_h_hex (car lista) (string-append "#" (convertir_rgb_hex_lista (hex (car lista))))) (descompress-formato-hex (cdr lista)))))))

; Dominio: formato de pixeles (list) X largo (int)
; Recorrido: string
; Descripción: Función que convierte el formato de pixeles en una cadena de string, pixhex->string
(define pixhex->string (lambda (formato_image image)

    ; Función que crea cadena de string
    (define fila_hex (lambda (formato_image fila contador image)
        (if (null? formato_image)
            "\n"
            (cond
              [(null? (car formato_image))
               (if (>= contador (cadr image))
                   (string-append "\n" (fila_hex formato_image (+ fila 1) 0 image))
                   (string-append "        " (fila_hex (cdr formato_image) fila (+ contador 1) image)))]

              [(= (x_hex (car formato_image)) fila)
               (string-append (hex (car formato_image)) " " (fila_hex (cdr formato_image) fila (+ contador 1) image))]

              [(>= contador (cadr image)) (string-append "\n" (fila_hex formato_image (+ fila 1) 0 image))]
              ))))


     ; Función que forma el string
     (define formar_string (lambda (formato_image image)
              (string-append (fila_hex formato_image 0 0 image))))
                         
    (formar_string formato_image image)))


; exportar la funcion al exterior
(provide (all-defined-out))


