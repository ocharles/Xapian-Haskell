#include "cxapian_queryparser.h"

Xapian::QueryParser *
queryparser_new()
{
    return new Xapian::QueryParser();
}

Xapian::QueryParser *
queryparser_copy(Xapian::QueryParser *other)
{
    return new Xapian::QueryParser( *other );
}

void
queryparser_delete(Xapian::QueryParser *self)
{
    delete self;
}

void
queryparser_set_stemmer (Xapian::QueryParser *self, Xapian::Stem *stemmer)
{
    self->set_stemmer(*stemmer);
}

void
queryparser_set_stemming_strategy (Xapian::QueryParser *self, int strategy)
{
    self->set_stemming_strategy( (Xapian::QueryParser::stem_strategy)strategy );
}

void
queryparser_set_stopper (Xapian::QueryParser *self, Xapian::Stopper *stop)
{
    self->set_stopper( stop );
}

// FIXME
//void
//queryparser_set_default_op (Xapian::QueryParser *, Query::op default_op);

// FIXME
//Query::op
//queryparser_get_default_op (Xapian::QueryParser *);

void 
queryparser_set_database (Xapian::QueryParser *self, Xapian::Database *db)
{
    self->set_database( *db );
}

// FIXME
//Xapian::Query *
//parse_query (Xapian::QueryParser *, std::string *query_string, unsigned flags=FLAG_DEFAULT, std::string *default_prefix);


Xapian::Query *
queryparser_parse_query_simple (Xapian::QueryParser *self, std::string *query_string)
{
    Xapian::Query *q = new Xapian::Query( self->parse_query(*query_string) );
    return q;
}

void 
queryparser_add_prefix (Xapian::QueryParser *self, std::string *field, std::string *prefix)
{
    self->add_prefix( *field, *prefix );
}

void 
queryparser_add_boolean_prefix (Xapian::QueryParser *self, std::string *field, std::string *prefix, cbool exclusive)
{
    self->add_boolean_prefix(*field, *prefix, (bool)exclusive);
}

//Xapian::TermIterator *
//queryparser_stoplist_begin (Xapian::QueryParser *);

//Xapian::TermIterator *
//queryparser_unstem_begin (Xapian::QueryParser*, std::string *term);

// FIXME
//void 
//queryparser_add_valuerangeprocessor (Xapian::QueryParser *, Xapian::ValueRangeProcessor *vrproc);

std::string *
queryparser_get_corrected_query_string (Xapian::QueryParser *self)
{
    std::string *str = new std::string( self->get_corrected_query_string());
    return str;
}

std::string *
queryparser_get_description (Xapian::QueryParser *self)
{
    std::string *str = new std::string( self->get_description() );
    return str;
}
