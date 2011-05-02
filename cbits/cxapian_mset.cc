#include <xapian.h>
#include "cxapian_mset.h"

mset *
mset_new ()
{
    mset *m = new mset();
    m->get  = new Xapian::MSet();
    return m;
}

mset *
mset_copy (mset *original)
{
    mset *m = new mset();
    m->get  = new Xapian::MSet(*original->get);
    return m;
}

void
mset_delete (mset *m)
{
    delete m->get;
    delete m;
}

void
mset_fetch_all (mset *m)
{
    m->get->fetch();
}

void
mset_fetch_one (mset *m, msetiterator *item)
{
    m->get->fetch(*item->iter);
}

void
mset_fetch_many (mset *m, msetiterator *begin, msetiterator *end)
{
    m->get->fetch(*begin->iter, *end->iter);
}

int
mset_convert_weight_to_percent (mset *m, double weight)
{
    return m->get->convert_to_percent((Xapian::weight)weight);
}

int
mset_convert_document_to_percent (mset *m, msetiterator *it)
{
    return m->get->convert_to_percent(*it->iter);
}

unsigned int
mset_get_termfreq (mset *m, const char *tname)
{
    return m->get->get_termfreq(std::string(tname));
}

double
mset_get_termweight (mset *m, const char *tname)
{
    return m->get->get_termweight(std::string(tname));
}

unsigned int
mset_get_firstitem (mset *m)
{
    return m->get->get_firstitem();
}

unsigned int
mset_get_matches_lower_bound (mset *m)
{
    return m->get->get_matches_lower_bound();
}

unsigned int
mset_get_matches_estimated (mset *m)
{
    return m->get->get_matches_estimated();
}

unsigned int
mset_get_matches_upper_bound (mset *m)
{
    return m->get->get_matches_upper_bound();
}

unsigned int
mset_get_uncollapsed_matches_lower_bound (mset *m)
{
    return m->get->get_uncollapsed_matches_lower_bound();
}

unsigned int
mset_get_uncollapsed_matches_estimated (mset *m)
{
    return m->get->get_uncollapsed_matches_estimated();
}

unsigned int
mset_get_uncollapsed_matches_upper_bound (mset *m)
{
    return m->get->get_uncollapsed_matches_upper_bound();
}

double // Xapian::weight
mset_get_max_possible (mset *m)
{
    return m->get->get_max_possible();
}

double // weight
mset_get_max_attained (mset *m)
{
    return m->get->get_max_attained();
}

unsigned int
mset_size (mset *m)
{
    return m->get->size();
}

unsigned int
mset_max_size (mset *m)
{
    return m->get->max_size();
}

cbool
mset_empty (mset *m)
{
    return m->get->empty();
}

void
mset_swap (mset *m, mset *other)
{
    return m->get->swap(*other->get);
}

msetiterator *
mset_begin (mset *m)
{
    msetiterator *mi = new msetiterator();
    mi->iter = new Xapian::MSetIterator(m->get->begin());
    return mi;
}

msetiterator *
mset_end (mset *m)
{
    msetiterator *mi = new msetiterator();
    mi->iter = new Xapian::MSetIterator(m->get->end());
    return mi;
}

msetiterator *
mset_back (mset *m)
{
    msetiterator *mi = new msetiterator();
    mi->iter = new Xapian::MSetIterator(m->get->back());
    return mi;
}

msetiterator *
mset_index (mset *m, unsigned int i)
{
    msetiterator *mi = new msetiterator();
    mi->iter = new Xapian::MSetIterator((*m->get)[i]);
    return mi;
}

const char *
mset_get_description (mset *m)
{
    return m->get->get_description().c_str();
}
