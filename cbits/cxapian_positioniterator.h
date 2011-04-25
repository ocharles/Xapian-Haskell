#ifndef CXAPIAN_POSITIONITERATOR
#define CXAPIAN_POSITIONITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

positioniterator *
positioniterator_new ();

void
positioniterator_next (positioniterator *);

unsigned int
positioniterator_get (positioniterator *);

bool
positioniterator_is_end (positioniterator *, positioniterator *end);

positioniterator *
positioniterator_copy (positioniterator *original);

void
positioniterator_delete (positioniterator *);

void
positioniterator_skip_to (positioniterator *, unsigned int pos);

const char *
positioniterator_get_description (positioniterator *);

}

#endif //CXAPIAN_POSITIONITERATOR
