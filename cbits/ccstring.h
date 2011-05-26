#ifndef CCSTRING
#define CCSTRING

#include <string>

extern "C" {

std::string *
toCCString_ (const char *source, unsigned int length);

const char *
fromCCString_ (std::string *source);

unsigned int
lengthCCString (std::string *source);

}

#endif //CCSTRING
