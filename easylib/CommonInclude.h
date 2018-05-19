#if !defined(COMMONINCLUDE_INCLUDE_)
#define COMMONINCLUDE_INCLUDE_

//为了方便，作为预编译头

//C++ Standand Libs
#pragma warning(disable: 4786)
#include <vector>
#include <list>
#include <set>
#include <map>
#include <string>
#include <fstream>
#include <sstream>
#include <algorithm>
using namespace std;

//C Libs needed
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <memory.h>
#include <malloc.h>

//depend on Win32 platform
#include <windows.h>

//common useful modules
#include "..\\include\\BasicType.h"
#include "..\\include\\WKSInclude.h"
#include "Persist.h"
#include "ClassFactory.h"
#include "SupportClasses.h"
#include "StringFuncs.h"

#endif
