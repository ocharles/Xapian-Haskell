#include <xapian.h>
#include "cxapian_positioniterator.h"

Xapian::PositionIterator *
positioniterator_new ()
{
    return new Xapian::PositionIterator();
}

void
positioniterator_next (Xapian::PositionIterator *self)
{
    (*self)++;
}

unsigned int
positioniterator_get (Xapian::PositionIterator *self)
{
    return (**self);
}

cbool
positioniterator_is_end (Xapian::PositionIterator *self, Xapian::PositionIterator* end)
{
    return (cbool)(*self == *end);
}

Xapian::PositionIterator *
positioniterator_copy (Xapian::PositionIterator *original)
{
    return new Xapian::PositionIterator(*original);
}

void
positioniterator_delete (Xapian::PositionIterator *self)
{
    delete self;
}

void
positioniterator_skip_to (Xapian::PositionIterator *self, unsigned int pos)
{
    self->skip_to((Xapian::termpos)pos);
}

const char *
positioniterator_get_description (Xapian::PositionIterator *self)
{
    return self->get_description().c_str();
}
