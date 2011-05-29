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
query_new_0 (std::string *term, unsigned int wqf, unsigned int termpos)
{
    return new Xapian::Query( *term, wqf, termpos );
}

Xapian::Query *
query_new_1 (int op, Xapian::Query *left, Xapian::Query *right)
{
    return new Xapian::Query((Xapian::Query::op) op, *left, *right);
}

Xapian::Query *
query_new_2 (int op, std::string *left, std::string *right)
{
    return new Xapian::Query((Xapian::Query::op) op, *left, *right);
}

Xapian::Query *
query_new_3 (int op, std::vector<Xapian::Query>::iterator *begin,
        std::vector<Xapian::Query>::iterator *end, unsigned int termcount)
{
    return new Xapian::Query((Xapian::Query::op) op, *begin, *end, termcount);
}

Xapian::Query *
query_new_4 (int op, Xapian::Query *subquery, double parameter)
{
    return new Xapian::Query((Xapian::Query::op) op,
                              *subquery,
                              parameter);
}

Xapian::Query *
query_new_5 (int op, unsigned int valno, std::string *lower, std::string *upper)
{
    return new Xapian::Query((Xapian::Query::op) op, valno, *lower, *upper);
}

Xapian::Query *
query_new_6 (int op, unsigned int valno, std::string *value)
{
    return new Xapian::Query((Xapian::Query::op)op,
                              (Xapian::valueno) valno,
                              *value);
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

std::string *
query_serialise (Xapian::Query *q)
{
    std::string *str = new std::string( q->serialise() );
    return str;
}

/*query_unserialise*/

std::string *
query_get_description (Xapian::Query *q)
{
    std::string *str = new std::string( q->get_description() );
    return str;
}
