#include <string>
#include "ccstring.h"

std::string *
ccstring_from_cstring (const char *source, unsigned int length)
{
    std::string *str = new std::string(source, length);
    return str;
}

const char *
ccstring_to_cstring (std::string *source)
{
    return source->c_str();
}

void
ccstring_delete (std::string *self)
{
    delete self;
}

unsigned int
ccstring_length (std::string *source)
{
    return source->length();
}
