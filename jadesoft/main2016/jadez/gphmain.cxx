#include <TApplication.h>
extern "C" {
void gphmain2_();
}
int main(int argc, char **argv) {
    TApplication theApp("App",&argc,argv);
    gphmain2_();
    theApp.Run();
    return 0;
}
