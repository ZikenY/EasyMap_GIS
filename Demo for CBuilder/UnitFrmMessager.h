//---------------------------------------------------------------------------

#ifndef UnitFrmMessagerH
#define UnitFrmMessagerH

#include <string>
using namespace std;

class TFrmMessager
{
public:
    virtual ~TFrmMessager(){};
    virtual long Dispatch(const string& msg) = 0;
};

//---------------------------------------------------------------------------
#endif
