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

Xapian::Query *
query_new ();

Xapian::Query *
query_copy (Xapian::Query *original);

void
query_delete (Xapian::Query *);

Xapian::Query * // wqf defaults to 1, termpos defaults to 0
query_new_0 (const char *tname, unsigned int wqf, unsigned int termpos);

Xapian::Query *
query_new_1 (int op, Xapian::Query *left, Xapian::Query *right);

Xapian::Query *
query_new_2 (int op, const char *left, const char *right);

/*
Xapian::Query *
query_new_3 (Xapian::Query *, int op, queryiterator *begin, queryiterator *end, unsigned int termcount);
*/

Xapian::Query *
query_new_4 (int op, Xapian::Query *subquery, double parameter);

/*
Xapian::Query *
query_new_5 (Xapian::Query *, int op, unsigned int valno, const char *begin...)

	Query(Query::op op_, Xapian::valueno valno,
	      const std::string &begin, const std::string &end);
*/

Xapian::Query *
query_new_6 (int op, unsigned int valno, const char *value);

/*
Xapian::Query *
query_new_7 (Xapian::Query *, postingsource *external_source);
*/

Xapian::Query *
query_match_all ();

Xapian::Query *
query_match_nothing ();

unsigned int
query_get_length (Xapian::Query *);

Xapian::TermIterator *
query_get_terms_begin (Xapian::Query *);

Xapian::TermIterator *
query_get_terms_end (Xapian::Query *);

cbool
query_empty (Xapian::Query *);

const char *
query_serialise (Xapian::Query *);

/*query_unserialise*/

const char *
query_get_description (Xapian::Query *);

}

#endif //CXAPIAN_QUERY
