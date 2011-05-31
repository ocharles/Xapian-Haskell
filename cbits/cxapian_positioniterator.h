#ifndef CXAPIAN_POSITIONITERATOR
#define CXAPIAN_POSITIONITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::PositionIterator *
positioniterator_new ();

void
positioniterator_next (Xapian::PositionIterator *);

unsigned int
positioniterator_get (Xapian::PositionIterator *);

cbool
positioniterator_is_end (Xapian::PositionIterator *, Xapian::PositionIterator *end);

Xapian::PositionIterator *
positioniterator_copy (Xapian::PositionIterator *original);

void
positioniterator_delete (Xapian::PositionIterator *);

void
positioniterator_skip_to (Xapian::PositionIterator *, unsigned int pos);

const char *
positioniterator_get_description (Xapian::PositionIterator *);

}

#endif //CXAPIAN_POSITIONITERATOR
