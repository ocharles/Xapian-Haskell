#ifndef CXAPIAN_STEM
#define CXAPIAN_STEM

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::Stem *
stem_copy (Xapian::Stem *original);

Xapian::Stem *
stem_new_with_language (const char *lang);

//stem_new_with_stemimplementation

void
stem_delete (Xapian::Stem *);

const char *
stem_word (Xapian::Stem *, const char *word);

const char *
stem_get_description (Xapian::Stem *);

const char *
stem_get_available_languages ();

}

#endif //CXAPIAN_STEM
