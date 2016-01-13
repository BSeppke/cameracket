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

(define pic_img   (grabimage cam))
(define pic       (image->racket-image pic_img))

(define (live-view t)
  (when (= (modulo t 10) 0)
    (begin
      (grabimage-unsafe! pic_img cam) 
      (set! pic (image->racket-image pic_img))))
  pic)

;;;Example usage:
;(animate live-view)


(define (test-speed [n 1000])
  (if (= n 0)
      "finished"
      (begin
        (time (grabimage-unsafe! pic_img))
        (test-speed (- n 1)))))

;;;Example usage:
;(test-speed)

(stop-camera-capture cam)