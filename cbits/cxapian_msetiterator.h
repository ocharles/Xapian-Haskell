#ifndef CXAPIAN_MSETITERATOR
#define CXAPIAN_MSETITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

msetiterator *
msetiterator_new ();

msetiterator *
msetiterator_copy (msetiterator *original);

void
msetiterator_delete (msetiterator *);

void  // I use void to emphasize the mutation involved
msetiterator_next (msetiterator *);

void // I ise void to emphasize the mutation involved
msetiterator_prev (msetiterator *);

cbool
msetiterator_is_end (msetiterator *, msetiterator *other);

unsigned int
msetiterator_get (msetiterator *);

document *
msetiterator_get_document (msetiterator *);

unsigned int
msetiterator_get_rank (msetiterator *);

unsigned int
msetiterator_get_weight (msetiterator *);

const char *
msetiterator_get_collapse_key (msetiterator *);

unsigned int
msetiterator_get_collapse_count (msetiterator *);

int
msetiterator_get_percent (msetiterator *);

const char *
msetiterator_get_description (msetiterator *);

}

#endif //CXAPIAN_MSETITERATOR
