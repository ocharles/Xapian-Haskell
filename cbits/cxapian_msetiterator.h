#ifndef CXAPIAN_MSETITERATOR
#define CXAPIAN_MSETITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::MSetIterator *
msetiterator_new ();

Xapian::MSetIterator *
msetiterator_copy (Xapian::MSetIterator *original);

void
msetiterator_delete (Xapian::MSetIterator *);

void  // I use void to emphasize the mutation involved
msetiterator_next (Xapian::MSetIterator *);

void // I ise void to emphasize the mutation involved
msetiterator_prev (Xapian::MSetIterator *);

cbool
msetiterator_is_end (Xapian::MSetIterator *, Xapian::MSetIterator *other);

unsigned int
msetiterator_get (Xapian::MSetIterator *);

Xapian::Document *
msetiterator_get_document (Xapian::MSetIterator *);

unsigned int
msetiterator_get_rank (Xapian::MSetIterator *);

unsigned int
msetiterator_get_weight (Xapian::MSetIterator *);

const char *
msetiterator_get_collapse_key (Xapian::MSetIterator *);

unsigned int
msetiterator_get_collapse_count (Xapian::MSetIterator *);

int
msetiterator_get_percent (Xapian::MSetIterator *);

const char *
msetiterator_get_description (Xapian::MSetIterator *);

}

#endif //CXAPIAN_MSETITERATOR
