#include <xapian.h>
#include "cxapian_msetiterator.h"

Xapian::MSetIterator *
msetiterator_new ()
{
    return new Xapian::MSetIterator();
}

Xapian::MSetIterator *
msetiterator_copy (Xapian::MSetIterator *original)
{
    return new Xapian::MSetIterator(*original);
}

void
msetiterator_delete (Xapian::MSetIterator *mi)
{
    delete mi;
}

void
msetiterator_next (Xapian::MSetIterator *mi)
{
    (*mi)++;
}

void
msetiterator_prev (Xapian::MSetIterator *mi)
{
    (*mi)--;
}

cbool
msetiterator_is_end (Xapian::MSetIterator *self, Xapian::MSetIterator *other)
{
    return (*self == *other);
}

unsigned int
msetiterator_get (Xapian::MSetIterator *mi)
{
    return **mi;
}

    Xapian::Document *
msetiterator_get_document (Xapian::MSetIterator *mi)
{
    return new Xapian::Document(mi->get_document());
}

unsigned int
msetiterator_get_rank (Xapian::MSetIterator *mi)
{
    return mi->get_rank();
}

unsigned int
msetiterator_get_weight (Xapian::MSetIterator *mi)
{
    return mi->get_weight();
}

const char *
msetiterator_get_collapse_key (Xapian::MSetIterator *mi)
{
    return mi->get_collapse_key().c_str();
}

unsigned int
msetiterator_get_collapse_count (Xapian::MSetIterator *mi)
{
    return mi->get_collapse_count();
}

int
msetiterator_get_percent (Xapian::MSetIterator *mi)
{
    return mi->get_percent();
}

const char *
msetiterator_get_description (Xapian::MSetIterator *mi)
{
    return mi->get_description().c_str();
}
