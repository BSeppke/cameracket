#lang racket

;; Module constants
(define cameracket-path (collection-path "cameracket"))
(define cameracket-version "1.0.0")

;; For windows, we need to find out, which architecture DrRacket is built
(require (only-in ffi/unsafe ctype-sizeof _pointer))
(define racket-bits (* 8 (ctype-sizeof _pointer)))

;; Load dll under windows, dylib under MacOS
(define cameracket-dylib-file
    (cond ((equal? (system-type 'os) 'windows) "opencv-grab_c.dll")
          ((equal? (system-type 'os) 'macosx)  "libopencv-grab_c.dylib")
          ((equal? (system-type 'os) 'unix)    "libopencv-grab_c.so")
          (else (error "Only macosx, windows and unix are supported"))))
(define cameracket-dylib-path (build-path cameracket-path cameracket-dylib-file))

;; Stuff needed to compile the c-bindings if necessary...
(define base_login_script "~/.profile")
(define opencv-grab_c-path (build-path cameracket-path "opencv-grab_c"))

(define login_script (if (file-exists? base_login_script)
                         base_login_script
                         (path->string (build-path opencv-grab_c-path "fallback.profile"))))

(define login_cmd (string-append "source " login_script))
(define (system-env arg) (system (string-append login_cmd " && " arg)))

(define (opencv-installed?)
  (display "Searching for OpenCV using 'pkg-config --modversion opencv': ")
  (system-env "pkg-config --modversion opencv"))

; The compilation routine (at least for macosx and unix)
(define (build-opencv-grab_c)
  (if (or (equal? (system-type 'os) 'macosx)
          (equal? (system-type 'os) 'unix))
      (if (opencv-installed?)
          ;;OPEN-CV is found!
          (begin 
            (display "-------------- BUILDING OpenCV-C-WRAPPER FOR CAMERA GRABBING --------------")
            (newline)
            (current-directory opencv-grab_c-path)
            (if (system-env (string-append " make " (symbol->string (system-type 'os)) (number->string racket-bits))) ; "make macosx32",  "make macosx64", "make unix32"  or "make unix64"
                (begin
                  (copy-file (build-path (current-directory) "bin" cameracket-dylib-file) cameracket-dylib-path)
                  #t)
                (error "making the opencv-grab_c lib failed, although OpenCV seems to be installed")))
          (error "OpenCV is not found. Please check if the prefix path is set correctly in /.profile file!"))
      ;;For windows
      (if (equal? (system-type 'os) 'windows)
          (let ((bindir     (build-path opencv-grab_c-path "bin" (string-append "win"(number->string racket-bits)))))
             (begin
               (system (string-append "copy " (path->string bindir) "\\*.dll " (path->string cameracket-path)))
               #t))
          (error "Only Mac OS X, Unix are supported for auto build of opencv-grab_c!"))))

;; Enable Auto-Build of the opencv-grab_c lib if not already present!
(when (not (file-exists? cameracket-dylib-path)) 
   (build-opencv-grab_c))
 
;; For Windows: Add the dll directory to the systems path:
(when (equal? (system-type 'os) 'windows)
    (putenv "PATH" (string-append (path->string cameracket-path) ";" (getenv "PATH"))))

(provide cameracket-path
         cameracket-version
         cameracket-dylib-path)

