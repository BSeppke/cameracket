#lang racket

(require vigracket)

(require cameracket/config)

(require ffi/unsafe)
(require ffi/cvector)


(define-cpointer-type _camera-capture)
  
;#################Get the capture pointer from opencv ###########
(define opencv_get_cam_capture_c
    (get-ffi-obj 'opencv_get_cam_capture_c cameracket-dylib-path
                (_fun [device : _int]
                      -> [capture_ptr : _camera-capture])))

(define opencv_release_cam_capture_c
  (get-ffi-obj 'opencv_release_cam_capture_c cameracket-dylib-path
                (_fun [capture_ptr : _camera-capture]
                      -> _void)))
  
(define opencv_get_cam_frame_width_ptr_c
    (get-ffi-obj 'opencv_get_cam_frame_width_ptr_c cameracket-dylib-path
                (_fun [capture_ptr : _camera-capture]
                      -> [width : _int])))

(define opencv_get_cam_frame_height_ptr_c
    (get-ffi-obj 'opencv_get_cam_frame_height_ptr_c cameracket-dylib-path
                (_fun [capture_ptr : _camera-capture]
                      -> [height : _int])))

(define opencv_get_cam_fps_ptr_c
    (get-ffi-obj 'opencv_get_cam_fps_ptr_c cameracket-dylib-path
                (_fun [capture_ptr : _camera-capture]
                      -> [height : _int])))

(define opencv_get_cam_frame_count_ptr_c
    (get-ffi-obj 'opencv_get_cam_frame_count_ptr_c cameracket-dylib-path
                (_fun [capture_ptr : _camera-capture]
                      -> [height : _int])))

(define opencv_grab_camera_rgbimage_ptr_c
  (get-ffi-obj 'opencv_grab_camera_rgbimage_ptr_c cameracket-dylib-path
               (_fun (img_vector_r img_vector_g img_vector_b width height device) :: [img_vector_r : _cvector]
                     [img_vector_g : _cvector]
                     [img_vector_b : _cvector]
                     [width : _int]
                     [height : _int]
                     [device : _camera-capture]
                     -> (res :  _int))))

(define (start-camera-capture [device_id 0])
  (opencv_get_cam_capture_c device_id))

(define (stop-camera-capture device_handle)
  (if (equal?  (cpointer-tag device_handle) 'camera-capture)
      (begin
        (opencv_release_cam_capture_c device_handle)
        (set-cpointer-tag! device_handle  'invalid))
       (error (string-append "Error in cameracket.grab.stop-camera-capture: incompatible capture used!\n"
                            "Expected: 'camera_capture\n"
                            "Got: '" (symbol->string (cpointer-tag device_handle))))))

(define (grabimage device_handle)
  (if (equal?  (cpointer-tag device_handle) 'camera-capture)
      (let* ((width (opencv_get_cam_frame_width_ptr_c   device_handle))
             (height (opencv_get_cam_frame_height_ptr_c device_handle))
             (img (make-image width height 3 0.0 0.0 0.0)))
        (grabimage-unsafe! img device_handle))
      (error (string-append "Error in cameracket.grab.grabimage: incompatible capture used!\n"
                            "Expected: 'camera_capture\n"
                            "Got: '" (symbol->string (cpointer-tag device_handle))))))

(define (grabimage-unsafe! img device_handle)
  (case (opencv_grab_camera_rgbimage_ptr_c (image-data img 0)  (image-data img 1)  (image-data img 2) (image-width img) (image-height img) device_handle)
    ((0) img)
    ((1) (error "Error in cameracket.grab.grabimage-unsafe!: Image cannot be grabbed by opencv!"))
    ((2) (error "Error in cameracket.grab.grabimage-unsafe!: Image is not RGB colored"))
    ((3) (error "Error in cameracket.grab.grabimage-unsafe!: Sizes do not match!"))))
  

(define image-grab         grabimage)
(define image-grab-unsafe! grabimage-unsafe!)

(provide    grabimage
            image-grab
            grabimage-unsafe!
            image-grab-unsafe!
            start-camera-capture
            stop-camera-capture)