#ifdef _WIN32
    #define LIBEXPORT extern "C" __declspec(dllexport) 
#else
    #define LIBEXPORT  extern "C" 
#endif