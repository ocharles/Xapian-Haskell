#ifndef CXAPIAN_ENQUIRE
#define CXAPIAN_ENQUIRE

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::Enquire * // no error handlers yet
enquire_new (Xapian::Database *db);

//Xapian::Enquire *
//enquire_copy (Xapian::Enquire *other);

void
enquire_delete (Xapian::Enquire *);

void // qlen defaults to 0
enquire_set_query (Xapian::Enquire *self, Xapian::Query *query, unsigned int qlen);

//query *
//enquire_get_query (Xapian::Enquire *self)

//void
//enquire_add_marchspy (Xapian::Enquire *self, matchspy *spy);

//void
//enquire_clear_matchspies (Xapian::Enquire *self);

Xapian::MSet *
enquire_get_mset (Xapian::Enquire *self, unsigned int first, unsigned int maxitems);

}


#endif //CXAPIAN_ENQUIRE
