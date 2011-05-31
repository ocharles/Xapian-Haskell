#ifndef CXAPIAN_MSET
#define CXAPIAN_MSET

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::MSet *
mset_new ();

Xapian::MSet *
mset_copy (Xapian::MSet *original);

void
mset_delete (Xapian::MSet *);

void
mset_fetch_all (Xapian::MSet *);

void
mset_fetch_one (Xapian::MSet *, Xapian::MSetIterator *item);

void
mset_fetch_many (Xapian::MSet *, Xapian::MSetIterator *begin, Xapian::MSetIterator *end);

int
mset_convert_weight_to_percent (Xapian::MSet *, double weight);

int
mset_convert_document_to_percent (Xapian::MSet *, Xapian::MSetIterator *other);

unsigned int
mset_get_termfreq (Xapian::MSet *, const char *tname);

double
mset_get_termweight (Xapian::MSet *, const char *tname);

unsigned int
mset_get_firstitem (Xapian::MSet *);

unsigned int
mset_get_matches_lower_bound (Xapian::MSet *);

unsigned int
mset_get_matches_estimated (Xapian::MSet *);

unsigned int
mset_get_matches_upper_bound (Xapian::MSet *);

unsigned int
mset_get_uncollapsed_matches_lower_bound (Xapian::MSet *);

unsigned int
mset_get_uncollapsed_matches_estimated (Xapian::MSet *);

unsigned int
mset_get_uncollapsed_matches_upper_bound (Xapian::MSet *);

double // Xapian::weight
mset_get_max_possible (Xapian::MSet *);

double // weight
mset_get_max_attained (Xapian::MSet *);

unsigned int
mset_size (Xapian::MSet *);

unsigned int
mset_max_size (Xapian::MSet *);

cbool
mset_empty (Xapian::MSet *);

void
mset_swap (Xapian::MSet *, Xapian::MSet *other);

Xapian::MSetIterator *
mset_begin (Xapian::MSet *);

Xapian::MSetIterator *
mset_end (Xapian::MSet *);

Xapian::MSetIterator *
mset_back (Xapian::MSet *);

Xapian::MSetIterator *
mset_index (Xapian::MSet *, unsigned int i); // operator[]

const char *
mset_get_description (Xapian::MSet *);

}

#endif //CXAPIAN_MSET
