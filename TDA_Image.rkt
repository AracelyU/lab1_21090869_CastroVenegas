#lang racket

;-----------------------------------TDA IMAGEN-----------------------------------------------------------------

;----------------------------------REPRESENTACION-------------------------------------------------------------

; El TDA representa una imagen del tipo rgb con diferentes formatos (bit, pixel, hexa).
; Se guarda una imagen como una lista de tres parametros
; (Width (int) X Height (int) X [pixbit-d* | pixrgb-d* | pixhex-d*]) que representan el ancho, largo
;  y el formato de la imagen respectivamente

;-----------------------------------CONSTRUCTORES-------------------------------------------------------------

; Dominio: entero X entero X [pixbit-d* | pixrgb-d* | pixhex-d*]
; Rec: Una representacion "image"
; Descripción: Función constructora de imágenes con bitmaps
; o pixmaps que incluye información de profundidad en cada pixel.
; Tipo de recursion: No se utiliza recursion de algun tipo

(define image (lambda (ancho largo . formato)
     (list ancho largo formato)))

; definir pixrgb-d <- x (int) X y (int) X r (C) X g (C) X b(C) X d (int)
(define pixrgb-d (lambda (x y c1 c2 c3 d)
               (list x y c1 c2 c3 d)))

; definir pixbit-d <- x (int) X y (int) X bit ([0|1]) X depth (int))
(define pixbit-d (lambda (x y bit d)
               (list x y bit d)))

; pixhex-d <- x (int) X y (int) X hex(String) X d (int)
(define pixhex-d (lambda (x y hex d)
               (list x y hex d)))
