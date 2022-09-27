#lang racket

(require "TDA_Image_21090869_CastroVenegas.rkt")
(require "TDA_Pixrgb-d_21090869_CastroVenegas.rkt")
(require "TDA_Pixbit-d_21090869_CastroVenegas.rkt")
(require "TDA_Pixhex-d_21090869_CastroVenegas.rkt")

; Creación de una imagen de 3 x 3 del tipo bitmap
(define image2 (image 3 3
                       (pixbit-d 0 0 0 10)
                       (pixbit-d 0 1 1 20)
                       (pixbit-d 0 2 1 30)
                       (pixbit-d 1 0 1 20)
                       (pixbit-d 1 1 0 30)
                       (pixbit-d 1 2 1 30)
                       (pixbit-d 2 0 0 20)
                       (pixbit-d 2 1 1 20)
                       (pixbit-d 2 2 0 40)))

(define image10 (crop image2 0 0 1 1)) ; debería retornar una imágen con cuatro pixeles