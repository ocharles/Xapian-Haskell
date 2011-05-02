#ifndef CXAPIAN_MSET
#define CXAPIAN_MSET

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

mset *
mset_new ();

mset *
mset_copy (mset *original);

void
mset_delete (mset *);

void
mset_fetch_all (mset *);

void
mset_fetch_one (mset *, msetiterator *item);

void
mset_fetch_many (mset *, msetiterator *begin, msetiterator *end);

int
mset_convert_weight_to_percent (mset *, double weight);

int
mset_convert_document_to_percent (mset *, msetiterator *it);

unsigned int
mset_get_termfreq (mset *, const char *tname);

double
mset_get_termweight (mset *, const char *tname);

unsigned int
mset_get_firstitem (mset *);

unsigned int
mset_get_matches_lower_bound (mset *);

unsigned int
mset_get_matches_estimated (mset *);

unsigned int
mset_get_matches_upper_bound (mset *);

unsigned int
mset_get_uncollapsed_matches_lower_bound (mset *);

unsigned int
mset_get_uncollapsed_matches_estimated (mset *);

unsigned int
mset_get_uncollapsed_matches_upper_bound (mset *);

double // Xapian::weight
mset_get_max_possible (mset *);

double // weight
mset_get_max_attained (mset *);

unsigned int
mset_size (mset *);

unsigned int
mset_max_size (mset *);

cbool
mset_empty (mset *);

void
mset_swap (mset *, mset *other);

msetiterator *
mset_begin (mset *);

msetiterator *
mset_end (mset *);

msetiterator *
mset_back (mset *);

msetiterator *
mset_index (mset *, unsigned int i); // operator[]

const char *
mset_get_description (mset *);

}

#endif //CXAPIAN_MSET
