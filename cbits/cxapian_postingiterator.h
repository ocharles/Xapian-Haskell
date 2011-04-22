#ifndef CXAPIAN_POSTINGITERATOR
#define CXAPIAN_POSTINGITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {


postingiterator *
postingiterator_new ();

postingiterator *
postingiterator_copy (postingiterator *other);

void
postingiterator_delete (postingiterator *);

unsigned int // Xapian::docid
postingiterator_get (postingiterator *);

void
postingiterator_skip_to (postingiterator *, unsigned int docid);

unsigned int
postingiterator_get_doclength (postingiterator *);

unsigned int
postingiterator_get_wdf (postingiterator *);

positioniterator *
postingiterator_positionlist_begin (postingiterator *);

positioniterator *
postingiterator_positionlist_end (postingiterator *);

const char *
postingiterator_get_description (postingiterator *);


}

#endif //CXAPIAN_POSTINGITERATOR
