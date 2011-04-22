#include <xapian.h>
#include "cxapian_query.h"

query *
query_new ()
{
    query *q = new query();
    q->get = new Xapian::Query();
    return q;
}

query *
query_copy (query *original)
{
    query *q = new query();
    q->get = new Xapian::Query(*original->get);
    return q;
}

void
query_delete (query *q)
{
    delete q->get;
    delete q;
}

query *
query_new_0 (const char *tname, unsigned int wqf, unsigned int termpos)
{
    query *self = new query();
    self->get = new Xapian::Query( std::string(tname), wqf, termpos );
    return self;
}

query *
query_new_1 (int op, query *left, query *right)
{
    query *self = new query();
    self->get = new Xapian::Query((Xapian::Query::op) op,
                               Xapian::Query(*left->get),
                               Xapian::Query(*right->get));
    return self;
}

query *
query_new_2 (int op, const char *left, const char *right)
{
    query *q = new query();
    q->get = new Xapian::Query((Xapian::Query::op) op,
                               std::string(left),
                               std::string(right));
    return q;
}

/*
query *
query_new_3 (query *, int op, queryiterator *begin, queryiterator *end, unsigned int termcount);
*/

query *
query_new_4 (int op, query *subquery, double parameter)
{
    query *q = new query();
    q->get = new Xapian::Query((Xapian::Query::op) op,
                               *subquery->get,
                               parameter);
    return q;
}

/*
query *
query_new_5 (query *, int op, unsigned int valno, const char *begin...)

	Query(Query::op op_, Xapian::valueno valno,
	      const std::string &begin, const std::string &end);
*/

query *
query_new_6 (int op, unsigned int valno, const char *value)
{
    query *q = new query();
    q->get = new Xapian::Query((Xapian::Query::op)op,
                               (Xapian::valueno) valno,
                               std::string(value));
    return q;
}

/*
query *
query_new_7 (query *, postingsource *external_source);
*/

query *
query_match_all ()
{
    query *q = new query();
    q->get = new Xapian::Query(Xapian::Query::MatchAll);
    return q;
}

query *
query_match_nothing ()
{
    query *q = new query();
    q->get = new Xapian::Query(Xapian::Query::MatchNothing);
    return q;
}

unsigned int
query_get_length (query *q)
{
    return q->get->get_length();
}

termiterator *
query_get_terms_begin (query *q)
{
    termiterator *termiter = new termiterator();
    termiter->iter = new Xapian::TermIterator(
                            q->get->get_terms_begin());
    return termiter;
}

termiterator *
query_get_terms_end (query *q)
{
    termiterator *termiter = new termiterator();
    termiter->iter = new Xapian::TermIterator(
                            q->get->get_terms_end());
    return termiter;
}

bool
query_empty (query *q)
{
    return q->get->empty();
}

const char *
query_serialise (query *q)
{
    return q->get->serialise().c_str();
}

/*query_unserialise*/

const char *
query_get_description (query *q)
{
    return q->get->get_description().c_str();
}
