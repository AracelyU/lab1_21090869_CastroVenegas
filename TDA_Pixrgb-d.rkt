#lang racket


; exportar la funcion al exterior
(provide (all-defined-out))

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


