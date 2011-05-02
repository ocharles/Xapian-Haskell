#include <xapian.h>
#include "cxapian_positioniterator.h"

positioniterator *
positioniterator_new ()
{
    positioniterator *self = new positioniterator();
    self->iter = new Xapian::PositionIterator();
    return self;
}

void
positioniterator_next (positioniterator *self)
{
    (*self->iter)++;
}

unsigned int
positioniterator_get (positioniterator *self)
{
    return (**self->iter);
}

cbool
positioniterator_is_end (positioniterator *self, positioniterator* end)
{
    return (*self->iter == *end->iter);
}

positioniterator *
positioniterator_copy (positioniterator *original)
{
    positioniterator *self = new positioniterator();
    self->iter = new Xapian::PositionIterator(*original->iter);
    return self;
}

void
positioniterator_delete (positioniterator *self)
{
    delete self->iter;
    delete self;
}

void
positioniterator_skip_to (positioniterator *self, unsigned int pos)
{
    self->iter->skip_to((Xapian::termpos)pos);
}

const char *
positioniterator_get_description (positioniterator *self)
{
    return self->iter->get_description().c_str();
}
