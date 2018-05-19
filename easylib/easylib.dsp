# Microsoft Developer Studio Project File - Name="easylib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=easylib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "easylib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "easylib.mak" CFG="easylib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "easylib - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "easylib - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "easylib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EASYLIB_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EASYLIB_EXPORTS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x804 /d "NDEBUG"
# ADD RSC /l 0x804 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Winmm.lib /nologo /dll /machine:I386 /out:"../bin/easylib.dll"

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EASYLIB_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "EASYLIB_EXPORTS" /FR /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x804 /d "_DEBUG"
# ADD RSC /l 0x804 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib Winmm.lib /nologo /dll /debug /machine:I386 /out:"../bin/easylib.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "easylib - Win32 Release"
# Name "easylib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\ActiveView.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\BitmapLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\CellNonupleTree.cpp
# End Source File
# Begin Source File

SOURCE=.\CellQuadTree.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\ClassFactory.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\cMap.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# ADD CPP /Yu"CommonInclude.h"

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\CommonInclude.cpp
# ADD CPP /Yc"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\CqOctree.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\CustomEditLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\dbfopen.c

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\dibapi.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\DisplayTransformation.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\DrawGeometry.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\easylib.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\easylib.def
# End Source File
# Begin Source File

SOURCE=.\ElementLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\ExtentAPI.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Fields.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\FileMapStream.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX /Yc /Yu

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\GeoMap.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\Geometry.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\GeometryLabel.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

# ADD CPP /Yu"CommonInclude.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\GeometryTracker.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\GroupLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\LabelLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\LayerAgent.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\MathLib.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\MemoryStream.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\MiniDisplay.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\MultiSymbol.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\NetTopoBase.cpp
# End Source File
# Begin Source File

SOURCE=.\Persist.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Plot3D.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\ScreenDisplay.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\ShapeAux.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\ShapeLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\shpopen.c

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SimpleFileStream.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SimpleSymbol.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\SlimLayer.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\Stream.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\StringFuncs.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# Begin Source File

SOURCE=.\SupportClasses.cpp

!IF  "$(CFG)" == "easylib - Win32 Release"

# SUBTRACT CPP /YX

!ELSEIF  "$(CFG)" == "easylib - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SymbolLib.cpp
# ADD CPP /Yu"CommonInclude.h"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\ActiveView.h
# End Source File
# Begin Source File

SOURCE=.\BitmapLayer.h
# End Source File
# Begin Source File

SOURCE=.\CellNonupleTree.h
# End Source File
# Begin Source File

SOURCE=.\CellQuadTree.h
# End Source File
# Begin Source File

SOURCE=.\ClassFactory.h
# End Source File
# Begin Source File

SOURCE=.\cMap.h
# End Source File
# Begin Source File

SOURCE=.\CommonInclude.h
# End Source File
# Begin Source File

SOURCE=.\CustomEditLayer.h
# End Source File
# Begin Source File

SOURCE=.\dibapi.h
# End Source File
# Begin Source File

SOURCE=.\Display.h
# End Source File
# Begin Source File

SOURCE=.\DisplayTransformation.h
# End Source File
# Begin Source File

SOURCE=.\DrawGeometry.h
# End Source File
# Begin Source File

SOURCE=.\easylib.h
# End Source File
# Begin Source File

SOURCE=.\ElementLayer.h
# End Source File
# Begin Source File

SOURCE=.\ExtentAPI.h
# End Source File
# Begin Source File

SOURCE=.\Fields.h
# End Source File
# Begin Source File

SOURCE=.\FileMapStream.h
# End Source File
# Begin Source File

SOURCE=.\GeoMap.h
# End Source File
# Begin Source File

SOURCE=.\Geometry.h
# End Source File
# Begin Source File

SOURCE=.\GeometryColumnInfo.h
# End Source File
# Begin Source File

SOURCE=.\GeometryLabel.h
# End Source File
# Begin Source File

SOURCE=.\GeometryTracker.h
# End Source File
# Begin Source File

SOURCE=.\GroupLayer.h
# End Source File
# Begin Source File

SOURCE=.\LabelLayer.h
# End Source File
# Begin Source File

SOURCE=.\LayerAgent.h
# End Source File
# Begin Source File

SOURCE=.\MathLib.h
# End Source File
# Begin Source File

SOURCE=.\MemoryStream.h
# End Source File
# Begin Source File

SOURCE=.\MiniDisplay.h
# End Source File
# Begin Source File

SOURCE=.\MultiSymbol.h
# End Source File
# Begin Source File

SOURCE=.\NetTopoBase.h
# End Source File
# Begin Source File

SOURCE=.\Persist.h
# End Source File
# Begin Source File

SOURCE=.\Plot3D.h
# End Source File
# Begin Source File

SOURCE=.\ScreenDisplay.h
# End Source File
# Begin Source File

SOURCE=.\ShapeAux.h
# End Source File
# Begin Source File

SOURCE=.\shapefil.h
# End Source File
# Begin Source File

SOURCE=.\ShapeLayer.h
# End Source File
# Begin Source File

SOURCE=.\SimpleFileStream.h
# End Source File
# Begin Source File

SOURCE=.\SimpleSymbol.h
# End Source File
# Begin Source File

SOURCE=.\SlimLayer.h
# End Source File
# Begin Source File

SOURCE=.\SpatialIndex.h
# End Source File
# Begin Source File

SOURCE=.\Stream.h
# End Source File
# Begin Source File

SOURCE=.\StringFuncs.h
# End Source File
# Begin Source File

SOURCE=.\SupportClasses.h
# End Source File
# Begin Source File

SOURCE=.\SymbolLib.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Group "include"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\Include\BasicType.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceActiveView.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceBitmapLayer.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceDisplay.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceDisplayTransformation.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceFields.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceGeometry.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceLabelLayer.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceLayer.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceLayerAgent.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceMap.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceObj.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfacePersist.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceStream.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceSupport.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceSymbol.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceTrackCancel.h
# End Source File
# Begin Source File

SOURCE=..\Include\InterfaceTracker.h
# End Source File
# Begin Source File

SOURCE=..\Include\MapUnits.h
# End Source File
# Begin Source File

SOURCE=..\Include\Messages.h
# End Source File
# Begin Source File

SOURCE=..\Include\WKSInclude.h
# End Source File
# End Group
# End Target
# End Project
