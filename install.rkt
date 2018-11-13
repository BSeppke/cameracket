#lang racket

(define local-collects-path
  (first (current-library-collection-paths)))

(define local-install-path  
  (build-path local-collects-path "cameracket"))


(define (show-notes)
  (display (format "cameracket has been successfully installed in ~a." local-install-path))(newline)
  (display "Use it by typing (require cameracket) or run the examples (provided in examples.rkt)")(newline)
  (cond ((equal? (system-type 'os) 'windows)
         (begin
           (display "=== Important notes for Windows ===")(newline)
           (display "Please make sure to install the MS Visual C++ 2012 runtimes first!")(newline)
           (display "You may get them from: https://www.microsoft.com/en-us/download/details.aspx?id=30679")(newline)))         
        ((equal? (system-type 'os) 'macosx)
         (begin
           (display "=== Important notes for Max OS X ===")(newline)
           (display "Please install the MacPorts port system first! You get it from: https://www.macports.org")(newline)
           (display "After the installation of MacPorts, install CMake and opencv by typing:")(newline)
           (display "    sudo port install cmake")(newline)
           (display "    sudo port install opencv")(newline)
           (display "The wrapper library opencv-grab_c will be build on the first load of the cameracket module!")(newline)))   
        ((equal? (system-type 'os) 'unix)
         (begin
           (display "=== Important notes for Linux ===")(newline)
           (display "Please make sure to install OpenCV first!")(newline)
           (display "The wrapper library opencv-grab_c will be build on the first load of the cameracket module!")(newline)))))

(define (install-cameracket)
  (begin
    ;; 1. Create local collects directory if not already existing
    (when (not (directory-exists? local-collects-path))
      (make-directory* local-collects-path))
    ;; 2. If cameracket-collects dir extist, delete it.
    (when (directory-exists? local-install-path)
      (delete-directory/files local-install-path))
    ;; 3. copy the installation contents to the local/collects/cameracket directory
    (copy-directory/files (current-directory) local-install-path)
    ;; 4. Tell the user about the status of the installation:
    (show-notes)))

;;Trigger the installation
(if (member (system-type 'os) '(windows macosx unix))
    (install-cameracket)
    (error "Sorry, but cameracket only supports Windows, Mac OS X or Linux"))