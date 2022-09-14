#lang racket


;-----------------------------------TDA PIXHEX-D -----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; El TDA representa pixel tipo rgb con parametros (posicion_x, posicion_y, string, profundidad).
; Se representa un pixel como una lista de custro parametros
; (posicionX, posicionY, formato_hexa, profundidad)

;-----------------------------------CONSTRUCTOR-------------------------------------------------------------

; Descripción: formato de un pixhex-d
; Dom: x int X y int X hexa int X d int
; Rec: pixhex-d
(define pixhex-d (lambda (x y hex d)
               (list x y hex d)))

;----------------------------------- SELECTORES--------------------------------------------------------------

; Descripción: obtener la posición x de un pixhex-d
; Dom: pixhex-d
; Rec: posición x (int)
(define x_hex (lambda (pixhex-d) (car pixhex-d)))

; Descripción: obtener la posicion y de un pixhex-d
; Dom: pixhex-d
; Rec: posición y (int)
(define y_hex (lambda (pixhex-d) (cadr pixhex-d)))

; Descripción: obtener el bit de un pixhex-d
; Dom: pixhex-d
; Rec: hexa (string)
(define hex (lambda (pixhex-d) (caddr pixhex-d)))

; Descripción: obtener la profundidad de un pixhex-d
; Dom: pixhex-d
; Rec: d (int)
(define d_hex (lambda (pixhex-d) (cadddr pixhex-d)))

; ---------------------------------- PERTENENCIA------------------------------------------------------------

; Descripción: función que verifica si el argumento es un pixhex-d
; Dom: x (int) X y (int) X hex (string) X d (int)
; Rec: Boleano
(define pixhex-d? (lambda (pixhex-d)
    (if (and (= (length pixhex-d) 4)
             (number? (x_hex pixhex-d))
             (number? (y_hex pixhex-d))
             (string? (hex pixhex-d))
             (number? (d_hex pixhex-d))

             ) #t #f)))

; Descripción: función que verifica si el argumento es un pixhex-d esta comprimida
; Dom: x (int) X y (int) X hex (string) X d (int)
; Rec: Boleano
(define pixhex-d_compressed? (lambda (pixhex-d)
    (if (and (= (length pixhex-d) 4)
             (number? (x_hex pixhex-d))
             (number? (y_hex pixhex-d))
             (list? (hex pixhex-d))
             (number? (d_hex pixhex-d))

             )#t #f)))

; Descripción: función que verifica si se comprimio un pixhex-d
; Dom: pixhex-d
; Rec: boleano
(define compress_hex? (lambda (pixhex-d)
     (if (string? (hex pixhex-d)) #t #f)))


; ----------------------------------- MODIFICADORES---------------------------------------------------------

; Descripción: modificar la posicion x de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_x_hex (lambda (pixhex-d_pasado x_nuevo)               
      (pixhex-d x_nuevo (y_hex pixhex-d_pasado) (hex pixhex-d_pasado) (d_hex pixhex-d_pasado))))


; Descripción: modificar la posicion y de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_y_hex (lambda (pixhex-d_pasado y_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) y_nuevo (hex pixhex-d_pasado) (d_hex pixhex-d_pasado))))


; Descripción: modificar el string hex de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_h_hex (lambda (pixhex-d_pasado h_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) (y_hex pixhex-d_pasado) h_nuevo (d_hex pixhex-d_pasado))))


; Descripción: modificar la profundidad de un pixhex-d
; Dom: pixhex-d
; Rec: pixhex-d
(define cambiar_d_hex (lambda (pixhex-d_pasado d_nuevo)               
      (pixhex-d (x_hex pixhex-d_pasado) (y_hex pixhex-d_pasado) (hex pixhex-d_pasado) d_nuevo)))



;--------------------------------------- SELECTORES ----------------------------------------------

; Descripción: función que filtra la lista por los elementos iguales a e
; Dom: lista x elemento
; Rec: lista
; tipo de recursión: Natural
(define filtro_iguales_hex (lambda (formato_image e)
    (if (null? formato_image)
        null
        (if (not (string-ci=? (hex (car formato_image)) e))
            (cons (car formato_image) (filtro_iguales_hex (cdr formato_image) e))
            (filtro_iguales_hex (cdr formato_image) e)))))

; Descripción: función que cuenta los elementos iguales a e en una lista
; Dom: lista (pixeles) x elemento
; Rec: lista
; tipo de recursión: cola
(define hex_iguales (lambda (formato_image e result)
       (if (null? formato_image)
           result
           (if (and (string-ci=? (hex (car formato_image)) e) )
               (hex_iguales (cdr formato_image) e (+ result 1))
               (hex_iguales (cdr formato_image) e result)))))


;---------------------------------------------------- OTROS -----------------------------------------------------------------------------------

; Descripción: función que recopila la cantidad de elemento de cada tipo de una lista
; Dom: lista (pixeles)
; Rec: lista
(define histograma_hex (lambda (formato_image)
    (if (null? formato_image)
        null
        (if (string-ci=? (hex (car formato_image)) "       ")
        (histograma_hex (filtro_iguales_hex formato_image (hex (car formato_image))))
        (cons (list (hex_iguales formato_image (hex (car formato_image)) 0)(hex (car formato_image)))
              (histograma_hex (filtro_iguales_hex formato_image (hex (car formato_image)))))))))

; Descripción: función que obtiene el hex más repetido del histograma
; Dom: lista del histograma
; Rec: lista
(define hex_mayor (lambda (lista_hex result)
    (if (null? lista_hex)
        (car (cdr result))
        (if (> (car(car lista_hex)) (car result))
            (hex_mayor (cdr lista_hex) (car lista_hex))
            (hex_mayor (cdr lista_hex) result)))))


; Dom: lista
; Rec: lista
; función que cambia un pixel hexa a rgb
(define convertir_hex_rgb (lambda (string_ingresado)

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

    (list
        (hex->rgb (obtener_valor (substring string_ingresado 0 2) 0) (- (string-length (substring string_ingresado 0 2)) 1) 0)
        (hex->rgb (obtener_valor (substring string_ingresado 2 4) 0) (- (string-length (substring string_ingresado 0 2)) 1) 0)
        (hex->rgb (obtener_valor (substring string_ingresado 4 6) 0) (- (string-length (substring string_ingresado 0 2)) 1) 0)
        )))




; Descripción: función que crea una lista sin el hex más repetido
(define compress-formato-hex (lambda (lista elemento)
     (if (null? lista)
         null
         (if (string-ci=? (hex (car lista)) elemento)
             (cons (cambiar_h_hex (car lista) (convertir_hex_rgb (hex (car lista)))) (compress-formato-hex (cdr lista) elemento))
             (cons (car lista) (compress-formato-hex (cdr lista) elemento))
             )
         )))



; Dom: Lista
; Rec: Lista
; función que cambia un pixel rgb a hexa
(define convertir_rgb_hex_lista (lambda (lista)
                            
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


   (string-append (rgb->hex (car lista)) (rgb->hex (cadr lista)) (rgb->hex (caddr lista)))))


(define a (list (list 0 0 (list 15 240 0) 10)
   (list 0 1 "#0000FF" 20)
   (list 0 2 "#00FF00" 30)
   (list 1 0 "#FFAOFF" 40)
   (list 1 1 "#FF12FF" 50)
   (list 1 2 "#F32FFF" 60)))

; Descripción: función que devuelve los valores perdidos tras compress
(define descompress-formato-hex (lambda (lista)
    (if (null? lista)
        null
        (if (string? (hex (car lista)))
            (cons (car lista) (descompress-formato-hex (cdr lista)))
            (cons (cambiar_h_hex (car lista) (string-append "#" (convertir_rgb_hex_lista (hex (car lista))))) (descompress-formato-hex (cdr lista)))

            ))))


; Descripción: pixhex->string
; Dom: formato image x largo image
; Rec: string
(define pixhex->string (lambda (formato_image largo)
                            
    (define fila_hex (lambda (formato_image fila)
        (if (null? formato_image)
            "\n"
            (if (= (x_hex (car formato_image)) fila)
                (string-append (hex (car formato_image)) " " (fila_hex (cdr formato_image) fila))
                (fila_hex (cdr formato_image) fila)))))

     (define formar_string (lambda (formato_image largo fila)
          (if (<= fila largo)
              (string-append (fila_hex formato_image fila) (formar_string formato_image largo (+ fila 1)))
              "\n")))
                         
    (formar_string formato_image largo 0)))






; exportar la funcion al exterior
(provide (all-defined-out))


