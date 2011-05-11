#ifndef CXAPIAN_VALUEITERATOR
#define CXAPIAN_VALUEITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::ValueIterator *
valueiterator_new ();

Xapian::ValueIterator *
valueiterator_copy (Xapian::ValueIterator *original);

void
valueiterator_delete (Xapian::ValueIterator *);

const char *
valueiterator_get (Xapian::ValueIterator *);

void  // I use void to emphasize the mutation involved
valueiterator_next (Xapian::ValueIterator *);

cbool
valueiterator_is_end (Xapian::ValueIterator *self, Xapian::ValueIterator *end);

unsigned int
valueiterator_get_docid (Xapian::ValueIterator *);

unsigned int
valueiterator_get_valueno (Xapian::ValueIterator *);

void
valueiterator_skip_to (Xapian::ValueIterator *, unsigned int docid_or_slot);

cbool
valueiterator_check (Xapian::ValueIterator *, unsigned int docid);

const char *
valueiterator_get_description (Xapian::ValueIterator *);

}

#endif //CXAPIAN_VALUEITERATOR
