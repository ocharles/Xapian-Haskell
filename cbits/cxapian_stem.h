#ifndef CXAPIAN_STEM
#define CXAPIAN_STEM

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

stem *
stem_copy (stem *original);

stem *
stem_new_with_language (const char *lang);

//stem_new_with_stemimplementation

void
stem_delete (stem *);

const char *
stem_word (stem *, const char *word);

const char *
stem_get_description (stem *);

const char *
stem_get_available_languages ();

}

#endif //CXAPIAN_STEM
