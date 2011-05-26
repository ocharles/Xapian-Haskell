#ifndef CCSTRING
#define CCSTRING

#include <string>

extern "C" {

std::string *
asCCString (const char *source, unsigned int length);

const char *
fromCCString (std::string *source);

unsigned int
lengthCCString (std::string *source);

}

#endif //CCSTRING
