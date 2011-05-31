#ifndef CXAPIAN_POSTINGITERATOR
#define CXAPIAN_POSTINGITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {


Xapian::PostingIterator *
postingiterator_new ();

Xapian::PostingIterator *
postingiterator_copy (Xapian::PostingIterator *other);

void
postingiterator_delete (Xapian::PostingIterator *);

void
postingiterator_next (Xapian::PostingIterator *);

unsigned int // Xapian::docid
postingiterator_get (Xapian::PostingIterator *);

cbool
postingiterator_is_end (Xapian::PostingIterator *pos
                       ,Xapian::PostingIterator *end);

void
postingiterator_skip_to (Xapian::PostingIterator *, unsigned int docid);

unsigned int
postingiterator_get_doclength (Xapian::PostingIterator *);

unsigned int
postingiterator_get_wdf (Xapian::PostingIterator *);

Xapian::PositionIterator *
postingiterator_positionlist_begin (Xapian::PostingIterator *);

Xapian::PositionIterator *
postingiterator_positionlist_end (Xapian::PostingIterator *);

const char *
postingiterator_get_description (Xapian::PostingIterator *);


}

#endif //CXAPIAN_POSTINGITERATOR
