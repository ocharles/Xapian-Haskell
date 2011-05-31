#ifndef CXAPIAN_TERMITERATOR
#define CXAPIAN_TERMITERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::TermIterator *
termiterator_new ();

Xapian::TermIterator *
termiterator_copy (Xapian::TermIterator *other);

void
termiterator_delete (Xapian::TermIterator *);

void
termiterator_next (Xapian::TermIterator *);

cbool
termiterator_is_end (Xapian::TermIterator *, Xapian::TermIterator *end);

std::string *
termiterator_get (Xapian::TermIterator *);

void
termiterator_skip_to (Xapian::TermIterator *, std::string *term);

unsigned int
termiterator_get_wdf (Xapian::TermIterator *);

unsigned int
termiterator_get_termfreq (Xapian::TermIterator *);

unsigned int
termiterator_positionlist_count (Xapian::TermIterator *);

Xapian::PositionIterator *
termiterator_positionlist_begin (Xapian::TermIterator *);

Xapian::PositionIterator *
termiterator_positionlist_end (Xapian::TermIterator *);

const char *
termiterator_get_description (Xapian::TermIterator *);

}

#endif //CXAPIAN_TERMITERATOR
