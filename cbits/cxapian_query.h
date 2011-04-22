#ifndef CXAPIAN_QUERY
#define CXAPIAN_QUERY

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

int OP_AND () {return (int)Xapian::Query::OP_AND;};
int OP_OR () {return (int)Xapian::Query::OP_OR;};
int OP_AND_NOT () {return (int)Xapian::Query::OP_AND_NOT;};
int OP_XOR () {return (int)Xapian::Query::OP_XOR;};
int OP_AND_MAYBE () {return (int)Xapian::Query::OP_AND_MAYBE;};
int OP_FILTER () {return (int)Xapian::Query::OP_FILTER;};
int OP_NEAR () {return (int)Xapian::Query::OP_NEAR;};
int OP_PHRASE () {return (int)Xapian::Query::OP_PHRASE;};
int OP_VALUE_RANGE () {return (int)Xapian::Query::OP_VALUE_RANGE;};
int OP_SCALE_WEIGHT () {return (int)Xapian::Query::OP_SCALE_WEIGHT;};
int OP_ELITE_SET () {return (int)Xapian::Query::OP_ELITE_SET;};
int OP_VALUE_GE () {return (int)Xapian::Query::OP_VALUE_GE;};
int OP_VALUE_LE () {return (int)Xapian::Query::OP_VALUE_LE;};
int OP_SYNONYM () {return (int)Xapian::Query::OP_SYNONYM;};

query *
query_new ();

query *
query_copy (query *original);

void
query_delete (query *);

query * // wqf defaults to 1, termpos defaults to 0
query_new_0 (const char *tname, unsigned int wqf, unsigned int termpos);

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
