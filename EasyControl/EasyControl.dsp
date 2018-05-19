# Microsoft Developer Studio Project File - Name="EasyControl" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=EasyControl - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "EasyControl.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EasyControl.mak" CFG="EasyControl - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EasyControl - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "EasyControl - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "EasyControl - Win32 Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Ext "ocx"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Ext "ocx"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_USRDLL" /FR /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x804 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x804 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 Winmm.lib /nologo /subsystem:windows /dll /machine:I386 /out:"../bin/EasyControl.ocx"
# Begin Custom Build - Registering ActiveX Control...
OutDir=.\Release
TargetPath=\easymap\pc\bin\EasyControl.ocx
InputPath=\easymap\pc\bin\EasyControl.ocx
SOURCE="$(InputPath)"

"$(OutDir)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	regsvr32 /s /c "$(TargetPath)" 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "EasyControl - Win32 Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Ext "ocx"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Ext "ocx"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_WINDLL" /D "_AFXDLL" /D "_MBCS" /D "_USRDLL" /FR /FD /GZ /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x804 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x804 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 Winmm.lib /nologo /subsystem:windows /dll /debug /machine:I386 /out:"../bin/EasyControl.ocx" /pdbtype:sept
# Begin Custom Build - Registering ActiveX Control...
OutDir=.\Debug
TargetPath=\easymap\pc\bin\EasyControl.ocx
InputPath=\easymap\pc\bin\EasyControl.ocx
SOURCE="$(InputPath)"

"$(OutDir)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	regsvr32 /s /c "$(TargetPath)" 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
	
# End Custom Build

!ENDIF 

# Begin Target

# Name "EasyControl - Win32 Release"
# Name "EasyControl - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\EasyClasses.cpp

!IF  "$(CFG)" == "EasyControl - Win32 Release"

# ADD CPP /Yu"StdAfx.h"

!ELSEIF  "$(CFG)" == "EasyControl - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EasyControl.cpp

!IF  "$(CFG)" == "EasyControl - Win32 Release"

# ADD CPP /Yu"StdAfx.h"

!ELSEIF  "$(CFG)" == "EasyControl - Win32 Debug"

# ADD CPP /Yu"stdafx.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EasyControl.def
# End Source File
# Begin Source File

SOURCE=.\EasyControl.odl
# ADD MTL /tlb "EasyControl.tlb"
# End Source File
# Begin Source File

SOURCE=.\EasyControl.rc
# End Source File
# Begin Source File

SOURCE=.\EasyControlCtl.cpp

!IF  "$(CFG)" == "EasyControl - Win32 Release"

# ADD CPP /Yu"StdAfx.h"

!ELSEIF  "$(CFG)" == "EasyControl - Win32 Debug"

# ADD CPP /Yu"stdafx.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EasyControlPpg.cpp

!IF  "$(CFG)" == "EasyControl - Win32 Release"

# ADD CPP /Yu"StdAfx.h"

!ELSEIF  "$(CFG)" == "EasyControl - Win32 Debug"

# ADD CPP /Yu"stdafx.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\RendererUI\RendererUI.cpp
# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp

!IF  "$(CFG)" == "EasyControl - Win32 Release"

# ADD CPP /Yc"StdAfx.h"

!ELSEIF  "$(CFG)" == "EasyControl - Win32 Debug"

# ADD CPP /Yc"stdafx.h"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\SymbolUI\SymbolUI.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\CommonFuncs.h
# End Source File
# Begin Source File

SOURCE=.\EasyClasses.h
# End Source File
# Begin Source File

SOURCE=.\EasyControl.h
# End Source File
# Begin Source File

SOURCE=.\EasyControlCtl.h
# End Source File
# Begin Source File

SOURCE=.\EasyControlPpg.h
# End Source File
# Begin Source File

SOURCE=..\RendererUI\RendererUI.h
# End Source File
# Begin Source File

SOURCE=.\Resource.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=..\SymbolUI\SymbolUI.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\CROSS.CUR
# End Source File
# Begin Source File

SOURCE=.\res\cursor1.cur
# End Source File
# Begin Source File

SOURCE=.\EasyControl.ico
# End Source File
# Begin Source File

SOURCE=.\EasyControlCtl.bmp
# End Source File
# Begin Source File

SOURCE=.\res\NORMAL.CUR
# End Source File
# Begin Source File

SOURCE=.\res\PAN.CUR
# End Source File
# Begin Source File

SOURCE=.\res\PANING.CUR
# End Source File
# Begin Source File

SOURCE=.\res\ZOOMIN.CUR
# End Source File
# Begin Source File

SOURCE=.\res\ZOOMOUT.CUR
# End Source File
# End Group
# Begin Group "include"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\Include\BasicType.h
# End Source File
# Begin Source File

SOURCE=..\Include\easylibdll.cpp
# End Source File
# Begin Source File

SOURCE=..\Include\easylibdll.h
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
# Begin Group "easylib"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\easylib\ActiveView.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ActiveView.h
# End Source File
# Begin Source File

SOURCE=..\easylib\BitmapLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\BitmapLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\CellNonupleTree.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\CellNonupleTree.h
# End Source File
# Begin Source File

SOURCE=..\easylib\CellQuadTree.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\CellQuadTree.h
# End Source File
# Begin Source File

SOURCE=..\easylib\ClassFactory.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ClassFactory.h
# End Source File
# Begin Source File

SOURCE=..\easylib\cMap.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\cMap.h
# End Source File
# Begin Source File

SOURCE=..\easylib\CommonInclude.h
# End Source File
# Begin Source File

SOURCE=..\easylib\CqOctree.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\CustomEditLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\CustomEditLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\dbfopen.c
# End Source File
# Begin Source File

SOURCE=..\easylib\dibapi.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\dibapi.h
# End Source File
# Begin Source File

SOURCE=..\easylib\Display.h
# End Source File
# Begin Source File

SOURCE=..\easylib\DisplayTransformation.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\DisplayTransformation.h
# End Source File
# Begin Source File

SOURCE=..\easylib\DrawGeometry.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\DrawGeometry.h
# End Source File
# Begin Source File

SOURCE=..\easylib\easylib.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\easylib.h
# End Source File
# Begin Source File

SOURCE=..\easylib\ElementLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ElementLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\ExtentAPI.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ExtentAPI.h
# End Source File
# Begin Source File

SOURCE=..\easylib\Fields.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\Fields.h
# End Source File
# Begin Source File

SOURCE=..\easylib\FileMapStream.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\FileMapStream.h
# End Source File
# Begin Source File

SOURCE=..\easylib\GeoMap.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\GeoMap.h
# End Source File
# Begin Source File

SOURCE=..\easylib\Geometry.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\Geometry.h
# End Source File
# Begin Source File

SOURCE=..\easylib\GeometryColumnInfo.h
# End Source File
# Begin Source File

SOURCE=..\easylib\GeometryLabel.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\GeometryLabel.h
# End Source File
# Begin Source File

SOURCE=..\easylib\GeometryTracker.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\GeometryTracker.h
# End Source File
# Begin Source File

SOURCE=..\easylib\GroupLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\GroupLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\LabelLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\LabelLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\LayerAgent.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\LayerAgent.h
# End Source File
# Begin Source File

SOURCE=..\easylib\MathLib.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\MathLib.h
# End Source File
# Begin Source File

SOURCE=..\easylib\MemoryStream.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\MemoryStream.h
# End Source File
# Begin Source File

SOURCE=..\easylib\MiniDisplay.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\MiniDisplay.h
# End Source File
# Begin Source File

SOURCE=..\easylib\MultiSymbol.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\MultiSymbol.h
# End Source File
# Begin Source File

SOURCE=..\easylib\NetTopoBase.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\NetTopoBase.h
# End Source File
# Begin Source File

SOURCE=..\easylib\Persist.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\Persist.h
# End Source File
# Begin Source File

SOURCE=..\easylib\Plot3D.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\Plot3D.h
# End Source File
# Begin Source File

SOURCE=..\easylib\ScreenDisplay.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ScreenDisplay.h
# End Source File
# Begin Source File

SOURCE=..\easylib\ShapeAux.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ShapeAux.h
# End Source File
# Begin Source File

SOURCE=..\easylib\shapefil.h
# End Source File
# Begin Source File

SOURCE=..\easylib\ShapeLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\ShapeLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\shpopen.c
# End Source File
# Begin Source File

SOURCE=..\easylib\SimpleFileStream.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\SimpleFileStream.h
# End Source File
# Begin Source File

SOURCE=..\easylib\SimpleSymbol.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\SimpleSymbol.h
# End Source File
# Begin Source File

SOURCE=..\easylib\SlimLayer.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\SlimLayer.h
# End Source File
# Begin Source File

SOURCE=..\easylib\SpatialIndex.h
# End Source File
# Begin Source File

SOURCE=..\easylib\Stream.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\Stream.h
# End Source File
# Begin Source File

SOURCE=..\easylib\StringFuncs.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\StringFuncs.h
# End Source File
# Begin Source File

SOURCE=..\easylib\SupportClasses.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\SupportClasses.h
# End Source File
# Begin Source File

SOURCE=..\easylib\SymbolLib.cpp
# End Source File
# Begin Source File

SOURCE=..\easylib\SymbolLib.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# End Target
# End Project
