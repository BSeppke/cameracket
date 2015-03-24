#lang racket

;; Module constants
(define cameracket-path (collection-path "cameracket"))
(define cameracket-version "1.1.0")

;; Load dll under windows, dylib under MacOS
(define cameracket-dylib-file
    (cond ((equal? (system-type 'os) 'windows) "opencv-grab_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libopencv-grab_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libopencv-grab_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define cameracket-dylib-path (build-path cameracket-path cameracket-dylib-file))

 
;; For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string cameracket-path) ";" (getenv "PATH"))))

(provide cameracket-path
         cameracket-version
         cameracket-dylib-path)

