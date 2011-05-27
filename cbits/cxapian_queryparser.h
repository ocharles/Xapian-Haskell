#ifndef CXAPIAN_QUERYPARSER
#define CXAPIAN_QUERYPARSER

#include <xapian.h>
#include "cxapian_types.h"


extern "C" {

int queryparser_STEM_NONE() {return (int)Xapian::QueryParser::STEM_NONE;}
int queryparser_STEM_SOME() {return (int)Xapian::QueryParser::STEM_SOME;}
int queryparser_STEM_ALL()  {return (int)Xapian::QueryParser::STEM_ALL;}

Xapian::QueryParser *
queryparser_new();

Xapian::QueryParser *
queryparser_copy(Xapian::QueryParser *other);

void
queryparser_delete(Xapian::QueryParser *);

void
queryparser_set_stemmer (Xapian::QueryParser *, Xapian::Stem *stemmer);

void
queryparser_set_stemming_strategy (Xapian::QueryParser *self, int strategy);

void
queryparser_set_stopper (Xapian::QueryParser *, Xapian::Stopper *stop);

// FIXME
//void
//queryparser_set_default_op (Xapian::QueryParser *, Query::op default_op);

// FIXME
//Query::op
//queryparser_get_default_op (Xapian::QueryParser *);

void 
queryparser_set_database (Xapian::QueryParser *, Xapian::Database *db);

// FIXME
//Xapian::Query *
//parse_query (Xapian::QueryParser *, std::string *query_string, unsigned flags=FLAG_DEFAULT, std::string *default_prefix);


Xapian::Query *
queryparser_parse_query_simple (Xapian::QueryParser *, std::string *query_string);

void 
queryparser_add_prefix (Xapian::QueryParser *, std::string *field, std::string *prefix);

void 
queryparser_add_boolean_prefix (Xapian::QueryParser *, std::string *field, std::string *prefix, cbool exclusive);

//Xapian::TermIterator *
//queryparser_stoplist_begin (Xapian::QueryParser *);

//Xapian::TermIterator *
//queryparser_unstem_begin (Xapian::QueryParser *, std::string *term);

// FIXME
//void 
//queryparser_add_valuerangeprocessor (Xapian::QueryParser *, Xapian::ValueRangeProcessor *vrproc);

std::string *
queryparser_get_corrected_query_string (Xapian::QueryParser *);

std::string *
queryparser_get_description (Xapian::QueryParser *);

}


#endif // CXAPIAN_QUERYPARSER
