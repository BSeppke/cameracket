#include "os_settings.hxx"

//Fwd declaration of CvCapture class:
class CvCapture;

/************ POINTER BASED FUNCTIONS *****************/

LIBEXPORT CvCapture*  opencv_get_cam_capture_c(int device_id=0);

LIBEXPORT void opencv_release_cam_capture_c(CvCapture* capture);

//Camera input (via OpenCV)
LIBEXPORT int opencv_grab_camera_rgbimage_ptr_c(float *arr_r, float *arr_g, float *arr_b, int width, int height, CvCapture* capture);

LIBEXPORT int opencv_get_cam_frame_height_ptr_c(CvCapture* capture);

LIBEXPORT int opencv_get_cam_frame_width_ptr_c(CvCapture* capture);

LIBEXPORT int opencv_get_cam_fps_ptr_c(CvCapture* capture);

LIBEXPORT int opencv_get_cam_frame_count_ptr_c(CvCapture* capture);


