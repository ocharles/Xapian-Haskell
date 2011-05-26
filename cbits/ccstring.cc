#include <string>
#include "ccstring.h"

std::string *
toCCString_ (const char *source, unsigned int length)
{
    std::string *str = new std::string(source, length);
    return str;
}

const char *
fromCCString_ (std::string *source)
{
    return source->c_str();
}

unsigned int
lengthCCString (std::string *source)
{
    return source->length();
}
