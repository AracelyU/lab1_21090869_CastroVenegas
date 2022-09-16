#lang racket

(require "TDA_Image.rkt")
(require "TDA_Pixrgb-d.rkt")
(require "TDA_Pixbit-d.rkt")
(require "TDA_Pixhex-d.rkt")

;------------------------------------------ IMAGENES ---------------------------------------------------------
; definir 4 pixeles de un pixrgb-d
(define pixrgb_1 (pixrgb-d 0 0 10 10 10 10)) ; lista_1
(define pixrgb_2 (pixrgb-d 0 1 20 20 20 20))
(define pixrgb_3 (pixrgb-d 0 2 30 30 30 20))
(define pixrgb_4 (pixrgb-d 1 0 40 40 40 20))
(define pixrgb_5 (pixrgb-d 1 1 50 50 50 30))
(define pixrgb_6 (pixrgb-d 1 2 60 60 60 30))

; definir una image 1
(define image_1 (image 3 2 pixrgb_1 pixrgb_2 pixrgb_3 pixrgb_4 pixrgb_5 pixrgb_6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; definir 4 pixeles de un pixbit-d
(define pixbit_1 (pixbit-d 0 0 0 10)) ; lista_2
(define pixbit_2 (pixbit-d 0 1 1 20))
(define pixbit_3 (pixbit-d 0 2 0 20))
(define pixbit_4 (pixbit-d 1 0 1 20))
(define pixbit_5 (pixbit-d 1 1 0 30))
(define pixbit_6 (pixbit-d 1 2 1 30))
(define pixbit_7 (pixbit-d 2 0 1 30))
(define pixbit_8 (pixbit-d 2 1 1 30))
(define pixbit_9 (pixbit-d 2 2 1 40))

; definir una image 2
(define image_2 (image 3 3 pixbit_1 pixbit_2 pixbit_3 pixbit_4 pixbit_5 pixbit_6 pixbit_7 pixbit_8 pixbit_9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
; definir 4 pixeles de un pixhex-d
(define pixhex_1 (pixhex-d 0 0 "#FF0000" 10)) ;lista_3
(define pixhex_2 (pixhex-d 0 1 "#0000FF" 20))
(define pixhex_3 (pixhex-d 0 2 "#00FF00" 20))
(define pixhex_4 (pixhex-d 1 0 "#FFAOFF" 20))
(define pixhex_5 (pixhex-d 1 1 "#FF12FF" 30))
(define pixhex_6 (pixhex-d 1 2 "#F32FFF" 30))

; definir una image 3
(define image_3 (image 3 2 pixhex_1 pixhex_2 pixhex_3 pixhex_4 pixhex_5 pixhex_6))



; Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)))



;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))



(define img3 (imgRGB->imgHex img1))
(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen



(define img18 (rotate90 img1))
(define img19 (rotate90 img2))
(define img20 (rotate90 img3))
(define img21 (rotate90 img4))
(define img22 (rotate90 img5))
(define img23 (rotate90 img6))
(define img24 (rotate90 img7))





