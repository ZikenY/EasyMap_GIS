//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
USEFORM("UnitFormImportShape.cpp", FormImportShape);
USEFORM("UnitFormSlimQueryResult.cpp", FormSlimQueryResult);
USEFORM("UnitFormSlimAttrib.cpp", FormSlimAttrib);
USEFORM("UnitFormMapAttrib.cpp", FormMapAttrib);
USEFORM("UnitFormCreateSlim.cpp", FormCreateSlim);
USEFORM("UnitFormEditBar.cpp", FormEditBar);
USEFORM("UnitFormMain.cpp", FormMain);
USEFORM("UnitFormLabelManager.cpp", FormLabelManager);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
    try
    {
         Application->Initialize();
         Application->Title = "YYGIS Desktop";
                 Application->CreateForm(__classid(TFormMain), &FormMain);
         Application->Run();
    }
    catch (Exception &exception)
    {
         Application->ShowException(&exception);
    }
    catch (...)
    {
         try
         {
             throw Exception("");
         }
         catch (Exception &exception)
         {
             Application->ShowException(&exception);
         }
    }
    return 0;
}
//---------------------------------------------------------------------------
