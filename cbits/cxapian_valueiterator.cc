#include <xapian.h>
#include "cxapian_valueiterator.h"

valueiterator *
valueiterator_new ()
{
    valueiterator *vi = new valueiterator();
    vi->iter = new Xapian::ValueIterator();
    return vi;
}

valueiterator *
valueiterator_copy (valueiterator *original)
{
    valueiterator *vi = new valueiterator();
    vi->iter = new Xapian::ValueIterator(*original->iter);
    return vi;
}

void
valueiterator_delete (valueiterator *vi)
{
    delete vi->iter;
    delete vi;
}

const char *
valueiterator_get (valueiterator *vi)
{
    return (**vi->iter).c_str();
}

valueiterator *
valueiterator_next (valueiterator *vi)
{
    vi->iter = vi->iter++; // TODO:does ++ mutate state?
    return vi;
}

unsigned int
valueiterator_get_docid (valueiterator *vi)
{
    return vi->iter->get_docid();
}

unsigned int
valueiterator_get_valueno (valueiterator *vi)
{
    return vi->iter->get_valueno();
}

void
valueiterator_skip_to (valueiterator *vi, unsigned int docid_or_slot)
{
    vi->iter->skip_to( docid_or_slot );
}

bool
valueiterator_check (valueiterator *vi, unsigned int docid)
{
    return vi->iter->check((Xapian::docid)docid);
}

const char *
valueiterator_get_description (valueiterator *vi)
{
    return vi->iter->get_description().c_str();
}
