#include <xapian.h>
#include "cxapian_postingiterator.h"

postingiterator *
postingiterator_new ()
{
    postingiterator *self = new postingiterator();
    self->iter = new Xapian::PostingIterator();
    return self;
}

postingiterator *
postingiterator_copy (postingiterator *other)
{
    postingiterator *self = new postingiterator();
    self->iter = new Xapian::PostingIterator(*other->iter);
    return self;
}

void
postingiterator_delete (postingiterator *self)
{
    delete self->iter;
    delete self;
}

unsigned int // Xapian::docid
postingiterator_get (postingiterator *self)
{
    return **self->iter;
}

void
postingiterator_skip_to (postingiterator *self, unsigned int docid)
{
    self->iter->skip_to(docid);
}

unsigned int
postingiterator_get_doclength (postingiterator *self)
{
    return self->iter->get_doclength();
}

unsigned int
postingiterator_get_wdf (postingiterator *self)
{
    return self->iter->get_wdf();
}

positioniterator *
postingiterator_positionlist_begin (postingiterator *self)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
            self->iter->positionlist_begin() );
    return pi;
}

positioniterator *
postingiterator_positionlist_end (postingiterator *self)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
            self->iter->positionlist_end() );
    return pi;
}

const char *
postingiterator_get_description (postingiterator *self)
{
    return self->iter->get_description().c_str();
}

