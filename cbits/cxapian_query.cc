#include <xapian.h>
#include "cxapian_query.h"

Xapian::Query *
query_new ()
{
    return new Xapian::Query();
}

Xapian::Query *
query_copy (Xapian::Query *original)
{
    return new Xapian::Query(*original);
}

void
query_delete (Xapian::Query *q)
{
    delete q;
}

Xapian::Query *
query_new_0 (const char *tname, unsigned int wqf, unsigned int termpos)
{
    return new Xapian::Query( std::string(tname), wqf, termpos );
}

Xapian::Query *
query_new_1 (int op, Xapian::Query *left, Xapian::Query *right)
{
    return new Xapian::Query((Xapian::Query::op) op,
                              Xapian::Query(*left),
                              Xapian::Query(*right));
}

Xapian::Query *
query_new_2 (int op, const char *left, const char *right)
{
    return new Xapian::Query((Xapian::Query::op) op,
                              std::string(left),
                              std::string(right));
}

/*
Xapian::Query *
query_new_3 (Xapian::Query *, int op, queryiterator *begin, queryiterator *end, unsigned int termcount);
*/

Xapian::Query *
query_new_4 (int op, Xapian::Query *subquery, double parameter)
{
    return new Xapian::Query((Xapian::Query::op) op,
                              *subquery,
                              parameter);
}

/*
Xapian::Query *
query_new_5 (Xapian::Query *, int op, unsigned int valno, const char *begin...)

	Query(Query::op op_, Xapian::valueno valno,
	      const std::string &begin, const std::string &end);
*/

Xapian::Query *
query_new_6 (int op, unsigned int valno, const char *value)
{
    return new Xapian::Query((Xapian::Query::op)op,
                              (Xapian::valueno) valno,
                              std::string(value));
}

/*
Xapian::Query *
query_new_7 (Xapian::Query *, postingsource *external_source);
*/

Xapian::Query *
query_match_all ()
{
    return new Xapian::Query(Xapian::Query::MatchAll);
}

Xapian::Query *
query_match_nothing ()
{
    return new Xapian::Query(Xapian::Query::MatchNothing);
}

unsigned int
query_get_length (Xapian::Query *q)
{
    return q->get_length();
}

Xapian::TermIterator *
query_get_terms_begin (Xapian::Query *q)
{
    return new Xapian::TermIterator(q->get_terms_begin());
}

Xapian::TermIterator *
query_get_terms_end (Xapian::Query *q)
{
    return new Xapian::TermIterator(q->get_terms_end());
}

cbool
query_empty (Xapian::Query *q)
{
    return q->empty();
}

const char *
query_serialise (Xapian::Query *q)
{
    return q->serialise().c_str();
}

/*query_unserialise*/

const char *
query_get_description (Xapian::Query *q)
{
    return q->get_description().c_str();
}
