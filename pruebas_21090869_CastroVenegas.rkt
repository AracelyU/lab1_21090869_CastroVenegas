#lang racket

(require "TDA_Image_21090869_CastroVenegas.rkt")
(require "TDA_Pixrgb-d_21090869_CastroVenegas.rkt")
(require "TDA_Pixbit-d_21090869_CastroVenegas.rkt")
(require "TDA_Pixhex-d_21090869_CastroVenegas.rkt")

;------------------------------------------  Script básico Pruebas ---------------------------------------------------------

#|
 En esta sección del archivo están todas las funciones del Script básico Pruebas

Script básico Pruebas
El código presentado a continuación contiene ejemplos que le permitirán probar todas las funciones. No obstante, estas
funciones no cubren todos los escenarios posibles. Estos ejemplos le servirán como referencia para su
autoevaluación, sin embargo se recomienda que pueda variar los ejemplos y probar distintos escenarios. Recuerde que en
su entrega final debe ampliar este script de pruebas con al menos 3 ejemplos más por función. Esto quiere decir, que su
script de prueba (archivo principal) debe contener todos los ejemplos listados a continuación (sin cambios) además de los suyos.

Las definiciones de imágenes enumeradas de 1 hasta N corresponden a funciones constantes. En este caso se aplican para
hacer el código más legible evitando una composición directa de las funciones, lo que hace que cada una de las evaluaciones
de funciones resulte más confusa. 

|#

(display "\nScript básico Pruebas - Documento\n\n\n")

; Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255 1)))



;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))


(define img3 (imgRGB->imgHex img1))


;imprimir una representación string de la imagen
(display (image->string img1 pixrgb->string))

;output:
; #FF0000 #00FF00
; #0000FF #FFFFFF


;imprimir una representación string de la imagen
(display (image->string img2 pixbit->string))

;output:
;0 1
;1 0

; El resto de los ejemplos, los puede obtener directamente desde las tablas presentadas en el enunciado. 

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t

(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f

(flipH img1)
(flipH img2)
(flipH img3)

(flipV img1)
(flipV img2)
(flipV img3)

(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen

(histogram img1)
(histogram img2)
(histogram img3)
(histogram img4)
(histogram img5)
(histogram img6)
(histogram img7)

(define img18 (rotate90 img1))
(define img19 (rotate90 img2))
(define img20 (rotate90 img3))
(define img21 (rotate90 img4))
(define img22 (rotate90 img5))
(define img23 (rotate90 img6))
(define img24 (rotate90 img7))

(define img8 (compress img1))
(define img9 (compress img2))
(define img10 (compress img3))
(define img11 (compress img4))
(define img12 (compress img5))
(define img13 (compress img6))
(define img14 (compress img7))

(compressed? img8)  ; la respuesta debería ser #t
(compressed? img9)  ; la respuesta debería ser #t
(compressed? img10)  ; la respuesta debería ser #t
(compressed? img11)  ; la respuesta debería ser #t
(compressed? img12)  ; la respuesta debería ser #t
(compressed? img13)  ; la respuesta debería ser #t
(compressed? img14)  ; la respuesta debería ser #t

(define img15 (edit invertColorBit img2))
(define img16 (edit invertColorRGB img1))

;se asume que las funciones de ajuste de canal están implementadas. 
;Puede cambiarlas por otras en su script de pruebas si así lo prefiere 
(define img33 (edit (adjustChannel getR setR incCh) img1))
(define img34 (edit (adjustChannel getG setG incCh) img1))
(define img35 (edit (adjustChannel getB setB incCh) img1))

;imágenes no comprimidas
(display (image->string img1 pixrgb->string))
(display (image->string img2 pixbit->string))
(display (image->string img3 pixhex->string))
(display (image->string img4 pixrgb->string))
(display (image->string img5 pixbit->string))
(display (image->string img6 pixrgb->string))
(display (image->string img7 pixbit->string))

;imagenes comprimidas, podrían internamente descomprimirlas para convertir a string ;(opcional)
(display (image->string img8 pixrgb->string))
(display (image->string img9 pixbit->string))
(display (image->string img10 pixhex->string)) 
(display (image->string img11 pixrgb->string))
(display (image->string img12 pixbit->string))
(display (image->string img13 pixrgb->string))
(display (image->string img14 pixbit->string))

;imágenes no comprimidas
(display (image->string img15 pixbit->string))
(display (image->string img16 pixrgb->string))
;(display (image->string img17 pixrgb->string)) # no existe la image 17
(display (image->string img18 pixrgb->string))
(display (image->string img19 pixbit->string))
(display (image->string img20 pixhex->string))
(display (image->string img21 pixrgb->string))
(display (image->string img22 pixbit->string))
(display (image->string img23 pixrgb->string))
(display (image->string img24 pixbit->string))

(depthLayers img1)
(depthLayers img2)
(depthLayers img3)
(depthLayers img4)
(depthLayers img5) 
(depthLayers img6) 
(depthLayers img7)

(define img25 (decompress img8))
(define img26 (decompress img9))
(define img27 (decompress img10))
(define img28 (decompress img11))
(define img29 (decompress img12))
(define img30 (decompress img13))
(define img31 (decompress img14))

;las siguientes comparaciones deberían arrojar #t
(equal? img25 img1)
(equal? img26 img2)
(equal? img27 img3)
(equal? img28 img4)
(equal? img29 img5)
(equal? img30 img6)
(equal? img31 img7)

;las siguientes comparaciones deberían arrojar #f
(equal? img25 img2)
(equal? img26 img1)

;------------------------------------------  Script básico Pruebas - Agregado ---------------------------------------------------------

#|
 En esta sección del archivo están todas las funciones del Script básico Pruebas adicionales

Script básico Pruebas
El código presentado a continuación contiene ejemplos que le permitirán probar todas las funciones. No obstante, estas
funciones no cubren todos los escenarios posibles. Es para ampliar este script de pruebas con al menos 3 ejemplos más por cada función

OBS:
-> Las imagenes definidas tendrán el formato imageX para diferenciarlas de las imagenes anteriores
-> en total salieron 31 imagenes definidas
|#

(display "\n\n\n\nScript básico Pruebas - Agregado\n\n\n")

; Creación de una imagen de 2 x 3 del tipo pixmap
(define image1 (image 2 3
                       (pixrgb-d 0 0 10 10 10 10)
                       (pixrgb-d 0 1 20 20 20 20)
                       (pixrgb-d 0 2 30 30 30 30)
                       (pixrgb-d 1 0 40 40 40 40)
                       (pixrgb-d 1 1 50 50 50 50)
                       (pixrgb-d 1 2 60 60 60 60)))

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

; Creación de una imagen de 2 x 3 del tipo hexmap
(define image3 (image 2 3
                       (pixhex-d 0 0 "#FF0000" 10)
                       (pixhex-d 0 1 "#0000FF" 20)
                       (pixhex-d 0 2 "#00FF00" 20)
                       (pixhex-d 1 0 "#00FF00" 10)
                       (pixhex-d 1 1 "#00FF00" 30)
                       (pixhex-d 1 2 "#F32FFF" 30)))


; probando funciones de pertenencia
(bitmap? image1) ; la respuesta debería ser #f
(bitmap? image2)  ; la respuesta debería ser #t
(bitmap? image3)  ; la respuesta debería ser #f

(pixmap? image1) ; la respuesta debería ser #t
(pixmap? image2)  ; la respuesta debería ser #f
(pixmap? image3)  ; la respuesta debería ser #f

(hexmap? image1) ; la respuesta debería ser #f
(hexmap? image2)  ; la respuesta debería ser #f
(hexmap? image3)  ; la respuesta debería ser #t

(compressed? image1) ; la respuesta debería ser #f
(compressed? image2) ; la respuesta debería ser #f
(compressed? image3) ; la respuesta debería ser #f


; probando funciones de flipH, flipV y rotate90
(flipH image1)
(flipH image2)
(flipH image3)

(flipV image1)
(flipV image2)
(flipV image3)

(define image4 (rotate90 image1))
(define image5 (rotate90 image2))
(define image6 (rotate90 image3))

; probando funcion crop
(define image7 (crop image1 1 2 0 1)) ; debería retornar una imágen con cuatro pixel
(define image8 (crop image2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define image9 (crop image3 0 2 1 1)) ; debería retornar una imágen con cuatro pixeles
(define image10 (crop image2 0 0 1 1)) ; debería retornar una imágen con cuatro pixeles

; probando imgRGB->imgHex
(define image11 (imgRGB->imgHex image1))
(define image12 (imgRGB->imgHex image4))
(define image13 (imgRGB->imgHex image7))

; probando histograma
(histogram image1)
;output:
;'((1 (10 10 10))
;  (1 (20 20 20))
;  (1 (30 30 30))
;  (1 (40 40 40))
;  (1 (50 50 50))
;  (1 (60 60 60)))

(histogram image2)
;output:
;'((4 0) (5 1))

(histogram image3)
;output:
;'((1 "#FF0000") (1 "#0000FF") (3 "#00FF00")(1 "#F32FFF"))

; probando image->string
(display (image->string image1 pixrgb->string))

;output:
;#0A0A0A #141414 #1E1E1E 
;#282828 #323232 #3C3C3C 

(display (image->string image6 pixhex->string))

;output:
;##00FF00 ##FF0000 
;##00FF00 ##0000FF 

(display (image->string image10 pixbit->string))

;output:
;0 1
;1 0 

; probando compress
(define image14 (compress image1))
(define image15 (compress image2))
(define image16 (compress image3))

(compressed? image14)  ; la respuesta debería ser #t
(compressed? image15)  ; la respuesta debería ser #t
(compressed? image16)  ; la respuesta debería ser #t

; probando funciones invertColorBit, invertColorRGB con edit
(define image17 (edit invertColorBit image2))
(define image18 (edit invertColorBit image5))
(define image19 (edit invertColorBit image8))

(define image20 (edit invertColorRGB image1))
(define image21 (edit invertColorRGB image4))
(define image22 (edit invertColorRGB image7))

;se asume que las funciones de ajuste de canal están implementadas. 
;Puede cambiarlas por otras en su script de pruebas si así lo prefiere 
(define image23 (edit (adjustChannel getR setR incCh) image1))
(define image24 (edit (adjustChannel getG setG incCh) image1))
(define image25 (edit (adjustChannel getB setB incCh) image1))


;imágenes no comprimidas
(display (image->string image1 pixrgb->string))
;output:
;#0A0A0A #141414 #1E1E1E 
;#282828 #323232 #3C3C3C 

(display (image->string image2 pixbit->string))
;output:
;0 1 1
;1 0 1
;0 1 0 

(display (image->string image3 pixhex->string))
;output:
;#FF0000 #0000FF #00FF00 
;#00FF00 #00FF00 #F32FFF 

;imagenes comprimidas, podrían internamente descomprimirlas para convertir a string ;(opcional)
(display (image->string image14 pixrgb->string))
;output:
;#0A0A0A #141414 #1E1E1E 
;#282828 #323232 #3C3C3C 

(display (image->string image15 pixbit->string))
;output:
;0 1 1
;1 0 1
;0 1 0

(display (image->string image16 pixhex->string))
;output:
;#FF0000 #0000FF #000FF0 
;#000FF0 #000FF0 #F32FFF 


;imágenes no comprimidas
(display (image->string image18 pixbit->string))
;output:
;1 0 1 
;0 1 0 
;1 0 0

(display (image->string image20 pixrgb->string))
;output:
;#F5F5F5 #EBEBEB #E1E1E1 
;#D7D7D7 #CDCDCD #C3C3C3

(display (image->string image11 pixhex->string))
;output:
;#0A0A0A #141414 #1E1E1E 
;#282828 #323232 #3C3C3C 

; probando depthLayers
(depthLayers image5) 
(depthLayers image6) 
(depthLayers image7)

; probando decompress
(define image26 (decompress image14))
(define image27 (decompress image15))
(define image28 (decompress image16))

; girando imagenes
(define image29 (flipH (flipH (flipH (flipH image3)))))
(define image30 (flipV (flipV image1)))
(define image31 (rotate90 (rotate90 (rotate90 (rotate90 image2)))))

;las siguientes comparaciones deberían arrojar #t
(equal? image26 image1)
(equal? image27 image2)
(equal? image29 image3)
(equal? image30 image1)

;las siguientes comparaciones deberían arrojar #f
(equal? image25 image2)
(equal? image13 image5)
(equal? image20 image3)