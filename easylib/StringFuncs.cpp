#include "CommonInclude.h"
#include "StringFuncs.h"

namespace easymap
{

Strings::Strings()
{
}

Strings::Strings(const string& text)
{
    this->SetText(text);
}

Strings::~Strings()
{
}

inline string trim_line(const string& line)
{
    if (line.size() < 1)
        return "";

    string r = line;
    if (r[0] == '\n' || r[0] == '\r')
    {
        r = r.substr(1, r.size() - 1);
    }

    if (r.size() < 1)
        return "";

    if (r[r.size() - 1] == '\n' || r[r.size() - 1] == '\r')
    {
        r = r.substr(0, r.size() - 1);
    }

    return r;
}

void Strings::SetText(const string& text)
{
    m_lines.clear();
    string sub = text;
    while (sub != "")
    {
        long rt = FindFirstChar(sub.c_str(), '\n');
        if (0 > rt)
        {
            if (sub.size() > 0)
            {
                m_lines.push_back(sub);
            }
            break;
        }

        string line = trim_line(sub.substr(0, rt));
        m_lines.push_back(line);
        sub = sub.substr(rt + 1, sub.size() - 1);
    }

}

string Strings::GetText() const
{
    vector<string>::const_iterator it = m_lines.begin();
    if (it == m_lines.end())
    {
        return "";
    }

    string text = *(it++);
    dword count = m_lines.size();
    for (dword i = 1; i < count; i++)
    {
        text = text + "\n" + *(it++);
    }

    return text;
}

dword Strings::GetLineCount() const
{
    return m_lines.size();
}

bool Strings::GetLine(const dword index, string& line) const
{
    if (m_lines.size() <= index)
    {
        return false;
    }

    line = m_lines[index];
    return true;
}

void Strings::AppendLine(const string& line)
{
    Strings strings(line);
    for (dword i = 0; i < strings.GetLineCount(); i++)
    {
        string l;
        strings.GetLine(i, l);
        m_lines.push_back(l);
    }
}

bool Strings::SetLine(const dword index, const string& line)
{
    if (m_lines.size() <= index)
    {
        return false;
    }

    Strings strings(line);
    if (strings.GetLineCount() <= 0)
    {
        return false;
    }

    string l;
    strings.GetLine(0, l);
    m_lines[index] = l;
    return true;
}

bool Strings::DeleteLine(const dword index)
{
    if (m_lines.size() <= index)
    {
        return false;
    }

    vector<string>::iterator it = m_lines.begin();
    for (dword i = 0; i < index; i++)
    {
        it++;  //??????????????????????????????
    }

    m_lines.erase(it);
    return true;
}



string TrimLeft(const string& s)
{
    long offset = -1;
    long size = s.size();
    for (long i = 0; i < size; i++)
    {
        if (' ' != s[i])
        {
            offset = i;
            break;
        }
    }

    if (0 > offset)
    {
        return string("");
    }

    return s.substr(offset, size);
}


string TrimRight(const string& s)
{
    long offset = -1;
    for (long i = s.size() - 1; i >= 0; i--)
    {
        if (' ' != s[i])
        {
            offset = i;
            break;
        }
    }

    if (0 > offset)
    {
        return string("");
    }

    return s.substr(0, offset + 1);
}

string Trim(const string& s)
{
    return TrimLeft(TrimRight(s));
}

string UpperString(const string& s)
{
    string t = s;
    return ::strupr((char*)t.c_str());
}

string LowerString(const string& s)
{
    string t = s;
    return ::strlwr((char*)t.c_str());
}

string IntToStr(const long i)
{
    static char cBuff[10];
    ::itoa(i, cBuff, 10);
    return string(cBuff);
}

long StrToInt(const string& s)
{
    return ::atoi(s.c_str());
}

string FloatToStr(const double f)
{
    static char cBuff[100];
    ::sprintf(cBuff, "%f", f);
    return string(cBuff);
}

double StrToFloat(const string& s)
{
    return ::atof(s.c_str());
}

long FindFirstChar(const char* pc, const char find)
{
    char* p = (char*)pc;
    long offset = -1;
    while (0 != *p)
    {
        if (find == *p)
        {
            offset = p - pc;
            break;
        }
        p++;
    }

    return offset;
}

long FindLastChar(const char* pc, const char find)
{
    char* p = (char*)pc;
    long offset = -1;
    while (0 != *p)
    {
        if (find == *p) offset = p - pc;
        p++;
    }

    return offset;
}

string GetDirectoryPart(const string& pathfilename)
{
    long offset = FindLastChar(pathfilename.c_str(), '\\');
    if (0 > offset)
    {
        return string("");
    }

    return string(pathfilename.substr(0, offset));
}

string RemoveDirectoryPart(const string& pathfilename)
{
    long offset = FindLastChar(pathfilename.c_str(), '\\');
    if (0 > offset)
    {
        return pathfilename;
    }

    return string(pathfilename.substr(offset + 1, pathfilename.length()));
}

string GetExtNamePart(const string& filename)
{
    long offset = FindLastChar(filename.c_str(), '.');
    if (0 > offset)
    {
        return string("");
    }

    return string(filename.substr(offset + 1, filename.length()));
}

string RemoveExtNamePart(const string& filename)
{
    long offset = FindLastChar(filename.c_str(), '.');
    if (0 > offset)
    {
        return filename;
    }

    return string(filename.substr(0, offset));
}

bool File2String(const string& filename, string& text)
{
    ifstream ifs(filename.c_str(), ios_base::in);
    if (!ifs.is_open())
    {
        return false;
    }

    ostringstream buf;
    char c;
    while (buf && ifs.get(c))
    {
        buf.put(c);
    }

    text = buf.str();
    return true;
}

bool String2File(const string& text, const string& filename)
{
    ofstream ofs(filename.c_str(), ios_base::out);
    if (!ofs.is_open())
    {
        return false;
    }

    long count = text.size();
    for (long i = 0; i < count; i++)
    {
        char c = text[i];
        ofs.put(c);
    }

    return true;
}



bool ini_setkeyvalue(string& text, const string& key, const string& value)
{
    Strings strings(text);

    long linecount = strings.GetLineCount();
    if (0 < linecount)
    {
        for (long i = 0; i< linecount; i++)
        {
            string line;
            strings.GetLine(i, line) ;
            long valuestart = key.size();
            string linehead = line.substr(0, valuestart);
            if (linehead == key)
            {
                strings.DeleteLine(i);
                break;
            }
        }
    }

    string keyval = key + " = " + value;
    strings.AppendLine(keyval);
    text = strings.GetText();
    return true;
}

bool ini_findkeyvalue(const string& text, const string& key, string& value)
{
    Strings strings(text);

    bool r = false;
    long linecount = strings.GetLineCount();
    if (0 < linecount)
    {
        for (long i = 0; i< linecount; i++)
        {
            string line;
            strings.GetLine(i, line) ;
            long valuestart = key.size();
            string linehead = line.substr(0, valuestart);
            if (linehead == key)
            {
                value = easymap::Trim(line.substr(valuestart, line.size()));
                value = easymap::Trim(value.substr(1, value.size()));
                r = true;
                break;
            }
        }
    }

    return r;
}

bool ini_deletekey(string& text, const string& key)
{
    Strings strings(text);
    bool r = false;

    long linecount = strings.GetLineCount();
    if (0 < linecount)
    {
        for (long i = 0; i< linecount; i++)
        {
            string line;
            strings.GetLine(i, line) ;
            long valuestart = key.size();
            string linehead = line.substr(0, valuestart);
            if (linehead == key)
            {
                strings.DeleteLine(i);
                r = true;
                break;
            }
        }
    }

    if (r)
    {
        text = strings.GetText();
    }

    return r;
}

}