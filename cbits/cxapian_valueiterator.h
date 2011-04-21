#ifndef CXAPIAN_VALUEITERATOR
#define CXAPIAN_VALUEITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

valueiterator *
valueiterator_new ();

valueiterator *
valueiterator_copy (valueiterator *original);

void
valueiterator_delete (valueiterator *);

const char *
valueiterator_get (valueiterator *);

void  // I use void to emphasize the mutation involved
valueiterator_next (valueiterator *);

unsigned int
valueiterator_get_docid (valueiterator *);

unsigned int
valueiterator_get_valueno (valueiterator *);

void
valueiterator_skip_to (valueiterator *, unsigned int docid_or_slot);

bool
valueiterator_check (valueiterator *, unsigned int docid);

const char *
valueiterator_get_description (valueiterator *);

}

#endif //CXAPIAN_VALUEITERATOR
