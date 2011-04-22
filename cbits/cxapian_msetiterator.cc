#include <xapian.h>
#include "cxapian_msetiterator.h"

msetiterator *
msetiterator_new ()
{
    msetiterator *mi = new msetiterator();
    mi->iter = new Xapian::MSetIterator();
    return mi;
}

msetiterator *
msetiterator_copy (msetiterator *original)
{
    msetiterator *mi = new msetiterator();
    mi->iter = new Xapian::MSetIterator(*original->iter);
    return mi;
}

void
msetiterator_delete (msetiterator *mi)
{
    delete mi->iter;
    delete mi;
}

void
msetiterator_next (msetiterator *mi)
{
    if (mi->iter)
        mi->iter = mi->iter++; // TODO: does this work?
}

void
msetiterator_prev (msetiterator *mi)
{
    if (mi->iter)
        mi->iter = mi->iter--; // TODO: does this work?
}

unsigned int
msetiterator_get (msetiterator *mi)
{
    return **mi->iter;
}

document *
msetiterator_get_document (msetiterator *mi)
{
    document *doc = new document();
    doc->get = new Xapian::Document(mi->iter->get_document());
    return doc;
}

unsigned int
msetiterator_get_rank (msetiterator *mi)
{
    return mi->iter->get_rank();
}

unsigned int
msetiterator_get_weight (msetiterator *mi)
{
    return mi->iter->get_weight();
}

const char *
msetiterator_get_collapse_key (msetiterator *mi)
{
    return mi->iter->get_collapse_key().c_str();
}

unsigned int
msetiterator_get_collapse_count (msetiterator *mi)
{
    return mi->iter->get_collapse_count();
}

int
msetiterator_get_percent (msetiterator *mi)
{
    return mi->iter->get_percent();
}

const char *
msetiterator_get_description (msetiterator *mi)
{
    return mi->iter->get_description().c_str();
}
