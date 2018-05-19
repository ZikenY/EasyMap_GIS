#if !defined(INTERFACETRACKCANCEL_INCLUDED_)
#define INTERFACETRACKCANCEL_INCLUDED_

#include "InterfacePersist.h"

namespace easymap
{
class ITrackCancel : public IPersist
{
public:
    virtual bool CheckCancel() const = 0;
    virtual void PostProgress(const long cacheid) const = 0;
    virtual void ShowHint(const long cacheid) const = 0;
};

typedef TSmartPtr<ITrackCancel> ITrackCancelPtr;
}

#endif