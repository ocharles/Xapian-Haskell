#include <xapian.h>
#include "cxapian_valueiterator.h"

Xapian::ValueIterator *
valueiterator_new ()
{
    return new Xapian::ValueIterator();
}

Xapian::ValueIterator *
valueiterator_copy (Xapian::ValueIterator *original)
{
    return new Xapian::ValueIterator(*original);
}

void
valueiterator_delete (Xapian::ValueIterator *self)
{
    delete self;
}

const char *
valueiterator_get (Xapian::ValueIterator *self)
{
    return (**self).c_str();
}

void
valueiterator_next (Xapian::ValueIterator *self)
{
    (*self)++;
}

cbool
valueiterator_is_end (Xapian::ValueIterator *self, Xapian::ValueIterator *end)
{
    return (*self== *end);
}

unsigned int
valueiterator_get_docid (Xapian::ValueIterator *self)
{
    return self->get_docid();
}

unsigned int
valueiterator_get_valueno (Xapian::ValueIterator *self)
{
    return self->get_valueno();
}

void
valueiterator_skip_to (Xapian::ValueIterator *self, unsigned int docid_or_slot)
{
    self->skip_to( docid_or_slot );
}

cbool
valueiterator_check (Xapian::ValueIterator *self, unsigned int docid)
{
    return self->check((Xapian::docid)docid);
}

const char *
valueiterator_get_description (Xapian::ValueIterator *self)
{
    return self->get_description().c_str();
}
