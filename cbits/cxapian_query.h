#ifndef CXAPIAN_QUERY
#define CXAPIAN_QUERY

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

query *
query_new ();

query *
query_copy (query *original);

void
query_delete (query *);

query *
query_new_1 (int op, query *left, query *right);

query *
query_new_2 (int op, const char *left, const char *right);

/*
query *
query_new_3 (query *, int op, queryiterator *begin, queryiterator *end, unsigned int termcount);
*/

query *
query_new_4 (int op, query *subquery, double parameter);

/*
query *
query_new_5 (query *, int op, unsigned int valno, const char *begin...)

	Query(Query::op op_, Xapian::valueno valno,
	      const std::string &begin, const std::string &end);
*/

query *
query_new_6 (int op, unsigned int valno, const char *value);

/*
query *
query_new_7 (query *, postingsource *external_source);
*/

query *
query_match_all ();

query *
query_match_nothing ();

unsigned int
query_get_length (query *);

termiterator *
query_get_terms_begin (query *);

termiterator *
query_get_terms_end (query *);

bool
query_empty (query *);

const char *
query_serialise (query *);

/*query_unserialise*/

const char *
query_get_description (query *);

}

#endif //CXAPIAN_QUERY
