// EasyControl.cpp : Implementation of CEasyControlApp and DLL registration.

#include "stdafx.h"
#include "EasyControl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


CEasyControlApp NEAR theApp;

const GUID CDECL BASED_CODE _tlid =
		{ 0x6607e61f, 0x2926, 0x45e6, { 0x90, 0xfe, 0xbe, 0xc1, 0x2b, 0x6c, 0xf9, 0x4e } };
const WORD _wVerMajor = 1;
const WORD _wVerMinor = 0;

const CEasyControlApp* GetApp()
{
    return &theApp;
}

////////////////////////////////////////////////////////////////////////////
// CEasyControlApp::InitInstance - DLL initialization

BOOL CEasyControlApp::InitInstance()
{
	BOOL bInit = COleControlModule::InitInstance();

	if (bInit)
	{

    }

	return bInit;
}


////////////////////////////////////////////////////////////////////////////
// CEasyControlApp::ExitInstance - DLL termination

int CEasyControlApp::ExitInstance()
{
	// TODO: Add your own module termination code here.

	return COleControlModule::ExitInstance();
}


/////////////////////////////////////////////////////////////////////////////
// DllRegisterServer - Adds entries to the system registry

STDAPI DllRegisterServer(void)
{
	AFX_MANAGE_STATE(_afxModuleAddrThis);

	if (!AfxOleRegisterTypeLib(AfxGetInstanceHandle(), _tlid))
		return ResultFromScode(SELFREG_E_TYPELIB);

	if (!COleObjectFactoryEx::UpdateRegistryAll(TRUE))
		return ResultFromScode(SELFREG_E_CLASS);

	return NOERROR;
}


/////////////////////////////////////////////////////////////////////////////
// DllUnregisterServer - Removes entries from the system registry

STDAPI DllUnregisterServer(void)
{
	AFX_MANAGE_STATE(_afxModuleAddrThis);

	if (!AfxOleUnregisterTypeLib(_tlid, _wVerMajor, _wVerMinor))
		return ResultFromScode(SELFREG_E_TYPELIB);

	if (!COleObjectFactoryEx::UpdateRegistryAll(FALSE))
		return ResultFromScode(SELFREG_E_CLASS);

	return NOERROR;
}
