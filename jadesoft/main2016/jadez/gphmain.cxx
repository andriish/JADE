#ifndef JEXTERNISCERNLIB
#include <TApplication.h>
extern "C" {
void gphmain2_();
}
#endif
int main(int argc, char **argv) {
#ifndef JEXTERNISCERNLIB
    TApplication theApp("App", &argc,argv);
    gphmain2_();
    theApp.Run();
#endif
    return 0;
}
