#lang racket

;Always the first step: load the VigRACKET lib and the CameRACKET lib...

(require vigracket)
(require cameracket)

(define cam (start-camera-capture))

(display "Enter A and press [ENTER] to take a picture: ")
(define uninteresting_input (read))
(define image (grabimage cam))

(show-image image)
;; Alternative (image->plt-image image)

(stop-camera-capture cam)