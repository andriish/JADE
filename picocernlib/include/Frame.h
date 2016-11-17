#include <TGFrame.h>
class TGWindow;
class TGMainFrame;
class TRootEmbeddedCanvas;
class Frame : public TGMainFrame {
public:
    TGMainFrame         *fMain;
    TRootEmbeddedCanvas *fEcanvas;
public:
    Frame(const TGWindow *p,UInt_t w,UInt_t h);
    virtual ~Frame();
    void DoDraw();
    virtual void CloseWindow();
    ClassDef(Frame,0)
};
