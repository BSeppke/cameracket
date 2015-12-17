#lang racket

;Always the first step: load the VigRACKET lib and the CameRACKET lib...

(require vigracket)
(require cameracket)

(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width plt-image-width)
                    (image-height plt-image-height)))
(require 2htdp/universe)

(define cam (start-camera-capture))

(define (grab+resize cam)
  (let* ((img   (grabimage cam))
         (new_w (inexact->exact (round (/ (image-width  img) 3))))
         (new_h (inexact->exact (round (/ (image-height img) 3)))))
  (resizeimage img new_w new_h 0)))


(define buf (image->racket-image (grab+resize cam)))

(define (live-view t)
  (when (= (modulo t 3) 0)
    (set! buf (image->racket-image (grab+resize cam))))
  buf)

(animate live-view)

(stop-camera-capture cam)