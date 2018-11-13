#include "opencv-grab_c.hxx"
#include "opencv2/highgui/highgui_c.h"


/************ POINTER BASED FUNCTIONS *****************/

CvCapture*  opencv_get_cam_capture_c(int device_id)
{ 
	CvCapture*  capture = cvCaptureFromCAM(device_id); // capture from video device
	if(!cvGrabFrame(capture))
	{              
		// capture a frame 
		cvReleaseCapture(&capture);
		return 0;
	}
	return capture;
}

void opencv_release_cam_capture_c(CvCapture* capture)
{ 
	cvReleaseCapture(&capture);
	free(capture);
	capture = NULL;
}

//Camera input (via OpenCV)
int opencv_grab_camera_rgbimage_ptr_c(float *arr_r, float *arr_g, float *arr_b, int width, int height, CvCapture* capture)
{ 
	IplImage* img = 0; 
	//CvCapture* capture = cvCaptureFromCAM(device_id); // capture from video device
	if(!capture || !cvGrabFrame(capture)){              // capture a frame 
		//cvReleaseCapture(&capture);
		return 1;
	}
	img=cvRetrieveFrame(capture);           // retrieve the captured frame
	
	cvQueryFrame(capture); 
	int cam_width =  cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH);
	int cam_height =  cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT);
	int cam_channels = img->nChannels;
	int step         = img->widthStep;
	unsigned char * data         = (unsigned char*)img->imageData;
	
	//cvReleaseCapture(&capture);
	
	// read image given as first argument
	// file type is determined automatically
	if( cam_width == width && cam_height == height){
		if(cam_channels == 3)
		{
			if(img->depth == IPL_DEPTH_8U)
			{	
				for(unsigned int y=0; y<cam_height; ++y)
				{
					for(unsigned int x=0; x<cam_width; ++x)
					{
						arr_r[y*width+x] = (float) data[y*step+x*cam_channels+2];
						arr_g[y*width+x] = (float) data[y*step+x*cam_channels+1];
						arr_b[y*width+x] = (float) data[y*step+x*cam_channels+0];
					}
				}
			}
			else
			{
				return 2;
			}
		}
		else 
		{
			return 3;
		}
	}	
	return 0;
}

int opencv_get_cam_frame_height_ptr_c(CvCapture* capture)
{
	cvQueryFrame(capture); 
	return (int) cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT);
}

int opencv_get_cam_frame_width_ptr_c(CvCapture* capture)
{
	
	cvQueryFrame(capture); 
	return (int) cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH);
}

int opencv_get_cam_fps_ptr_c(CvCapture* capture)
{
	
	cvQueryFrame(capture); 
	return (int) cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);
}

int opencv_get_cam_frame_count_ptr_c(CvCapture* capture)
{
	
	cvQueryFrame(capture); 
	return (int) cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_COUNT);
}


