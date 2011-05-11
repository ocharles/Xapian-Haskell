#include <xapian.h>
#include "cxapian_mset.h"

Xapian::MSet *
mset_new ()
{
    return new Xapian::MSet();
}

Xapian::MSet *
mset_copy (Xapian::MSet *original)
{
    return new Xapian::MSet(*original);
}

void
mset_delete (Xapian::MSet *self)
{
    delete self;
}

void
mset_fetch_all (Xapian::MSet *self)
{
    self->fetch();
}

void
mset_fetch_one (Xapian::MSet *self, Xapian::MSetIterator *item)
{
    self->fetch(*item);
}

void
mset_fetch_many (Xapian::MSet *self, Xapian::MSetIterator *begin, Xapian::MSetIterator *end)
{
    self->fetch(*begin, *end);
}

int
mset_convert_weight_to_percent (Xapian::MSet *self, double weight)
{
    return self->convert_to_percent((Xapian::weight)weight);
}

int
mset_convert_document_to_percent (Xapian::MSet *self, Xapian::MSetIterator *other)
{
    return self->convert_to_percent(*other);
}

unsigned int
mset_get_termfreq (Xapian::MSet *self, const char *tname)
{
    return self->get_termfreq(std::string(tname));
}

double
mset_get_termweight (Xapian::MSet *self, const char *tname)
{
    return self->get_termweight(std::string(tname));
}

unsigned int
mset_get_firstitem (Xapian::MSet *self)
{
    return self->get_firstitem();
}

unsigned int
mset_get_matches_lower_bound (Xapian::MSet *self)
{
    return self->get_matches_lower_bound();
}

unsigned int
mset_get_matches_estimated (Xapian::MSet *self)
{
    return self->get_matches_estimated();
}

unsigned int
mset_get_matches_upper_bound (Xapian::MSet *self)
{
    return self->get_matches_upper_bound();
}

unsigned int
mset_get_uncollapsed_matches_lower_bound (Xapian::MSet *self)
{
    return self->get_uncollapsed_matches_lower_bound();
}

unsigned int
mset_get_uncollapsed_matches_estimated (Xapian::MSet *self)
{
    return self->get_uncollapsed_matches_estimated();
}

unsigned int
mset_get_uncollapsed_matches_upper_bound (Xapian::MSet *self)
{
    return self->get_uncollapsed_matches_upper_bound();
}

double // Xapian::weight
mset_get_max_possible (Xapian::MSet *self)
{
    return self->get_max_possible();
}

double // weight
mset_get_max_attained (Xapian::MSet *self)
{
    return self->get_max_attained();
}

unsigned int
mset_size (Xapian::MSet *self)
{
    return self->size();
}

unsigned int
mset_max_size (Xapian::MSet *self)
{
    return self->max_size();
}

cbool
mset_empty (Xapian::MSet *self)
{
    return self->empty();
}

void
mset_swap (Xapian::MSet *self, Xapian::MSet *other)
{
    return self->swap(*other);
}

Xapian::MSetIterator *
mset_begin (Xapian::MSet *self)
{
    return new Xapian::MSetIterator(self->begin());
}

Xapian::MSetIterator *
mset_end (Xapian::MSet *self)
{
    return new Xapian::MSetIterator(self->end());
}

Xapian::MSetIterator *
mset_back (Xapian::MSet *self)
{
    return new Xapian::MSetIterator(self->back());
}

Xapian::MSetIterator *
mset_index (Xapian::MSet *self, unsigned int i)
{
    return new Xapian::MSetIterator((*self)[i]);
}

const char *
mset_get_description (Xapian::MSet *self)
{
    return self->get_description().c_str();
}
