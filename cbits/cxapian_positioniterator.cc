#include <xapian.h>
#include "cxapian_types.h"

positioniterator *
positioniterator_new ()
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator();
    return pi;
}

positioniterator *
positioniterator_copy (positioniterator *original)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(*original->iter);
    return pi;
}

void
positioniterator_delete (positioniterator *pi)
{
    delete pi->iter;
    delete pi;
}

void
positioniterator_skip_to (positioniterator *pi, unsigned int pos)
{
    pi->iter->skip_to((Xapian::termpos)pos);
}

const char *
positioniterator_get_description (positioniterator *pi)
{
    return pi->iter->get_description().c_str();
}
