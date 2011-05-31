#ifndef CCSTRING
#define CCSTRING

#include <string>

extern "C" {

std::string *
ccstring_from_cstring (const char *source, unsigned int length);

const char *
ccstring_to_cstring (std::string *source);

void
ccstring_delete (std::string *);

unsigned int
ccstring_length (std::string *source);

}

#endif //CCSTRING
