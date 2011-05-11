#include <xapian.h>
#include "cxapian_postingiterator.h"

Xapian::PostingIterator *
postingiterator_new ()
{
    return new Xapian::PostingIterator();
}

Xapian::PostingIterator *
postingiterator_copy (Xapian::PostingIterator *other)
{
    return new Xapian::PostingIterator(*other);
}

void
postingiterator_delete (Xapian::PostingIterator *self)
{
    delete self;
}

unsigned int // Xapian::docid
postingiterator_get (Xapian::PostingIterator *self)
{
    return **self;
}

void
postingiterator_skip_to (Xapian::PostingIterator *self, unsigned int docid)
{
    self->skip_to(docid);
}

unsigned int
postingiterator_get_doclength (Xapian::PostingIterator *self)
{
    return self->get_doclength();
}

unsigned int
postingiterator_get_wdf (Xapian::PostingIterator *self)
{
    return self->get_wdf();
}

Xapian::PositionIterator *
postingiterator_positionlist_begin (Xapian::PostingIterator *self)
{
    return new Xapian::PositionIterator(
            self->positionlist_begin() );
}

Xapian::PositionIterator *
postingiterator_positionlist_end (Xapian::PostingIterator *self)
{
    return new Xapian::PositionIterator(
            self->positionlist_end() );
}

const char *
postingiterator_get_description (Xapian::PostingIterator *self)
{
    return self->get_description().c_str();
}

