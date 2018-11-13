#lang racket

(require ffi/unsafe)

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

 
;; The compilation routine (at least for macosx and unix)
(define (rebuild-cameracket)
  (define old_path (current-directory))
  (define opencv-grab_c-path (build-path cameracket-path "opencv-grab_c"))
  (define racket-bits (* 8 (ctype-sizeof _pointer)))
  (begin
    (if (or (equal? (system-type 'os) 'macosx)
            (equal? (system-type 'os) 'unix))
        (begin 
          (display "-------------- BUILDING OPENCV C-WRAPPER FOR CAMERA GRABBING --------------")
          (newline)
          (current-directory opencv-grab_c-path)
          (if (system* (format "build-~a.sh" racket-bits))
              (begin
                (copy-file (build-path (current-directory) "bin"  cameracket-dylib-file) cameracket-dylib-path #t)
                #t)
              (error "making the opencv-grab_c lib failed, although vigra seems to be installed"))
          (current-directory old_path))
        ;;For windows: Just copy the correct binaries (no system called needed herein
        (let ((bindir     (build-path opencv-grab_c-path "bin" (string-append "win"(number->string racket-bits)))))
          (if (equal? (system-type 'os) 'windows)
              (let* ((binaries   (find-files (compose (curry equal?  #".dll") path-get-extension)  bindir))
                     (result     (map (lambda (f) (copy-file f (path->string (build-path cameracket-path (file-name-from-path f))) #t)) binaries)))
                (if (foldl (lambda (x y) (and x y)) #t result)
                    #t
                    (error (string-append "Copying of opencv-grab_c's binaries from " (path->string bindir) " to "  (path->string cameracket-path) " failed !"))))
              (error "Only windows, Mac OS X and Unix are supported!"))))
    ;;If it still fails loading -> give up and inform the user
    (void (ffi-lib cameracket-dylib-path #:fail (lambda() (error "The opencv-grab_c (still) cannot be loaded after one rebuild phase"))))))


;; For Windows: Add the dll directory to the systems path:
(void (when (equal? (system-type 'os) 'windows)
        (putenv "PATH" (string-append (path->string cameracket-path) ";" (getenv "PATH")))))

;; For Mac OS X: Add the MacPorts directory to the systems path
(void (when (equal? (system-type 'os) 'macosx)
        (putenv "PATH" (string-append "/opt/local/bin" ":" (getenv "PATH")))))

;;Try to load the dylib: If this fails - try to rebuild it!
(void (ffi-lib cameracket-dylib-path #:fail rebuild-cameracket))

(provide cameracket-path
         cameracket-version
         cameracket-dylib-path)

