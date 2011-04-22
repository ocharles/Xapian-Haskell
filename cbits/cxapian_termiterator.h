#ifndef CXAPIAN_TERMITERATOR
#define CXAPIAN_TERMITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

termiterator *
termiterator_new ();

termiterator *
termiterator_copy (termiterator *other);

void
termiterator_delete (termiterator *);

void
termiterator_next (termiterator *self)
{
    (*self->iter)++;
}

bool
termiterator_is_end (termiterator *self, termiterator *end)
{
    return *self->iter == *end->iter;
}

const char *
termiterator_get (termiterator *);

void
termiterator_skip_to (termiterator *, const char *tname);

unsigned int
termiterator_get_wdf (termiterator *);

unsigned int
termiterator_get_termfreq (termiterator *);

unsigned int
termiterator_positionlist_count (termiterator *);

positioniterator *
termiterator_positionlist_begin (termiterator *);

positioniterator *
termiterator_positionlist_end (termiterator *);

const char *
termiterator_get_description (termiterator *);

}

#endif //CXAPIAN_TERMITERATOR
