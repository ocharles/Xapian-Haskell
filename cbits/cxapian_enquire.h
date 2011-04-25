#ifndef CXAPIAN_ENQUIRE
#define CXAPIAN_ENQUIRE

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

enquire * // no error handlers yet
enquire_new (database *db);

//enquire *
//enquire_copy (enquire *other);

void
enquire_delete (enquire *);

void // qlen defaults to 0
enquire_set_query (enquire *self, query *query, unsigned int qlen);

//query *
//enquire_get_query (enquire *self)

//void
//enquire_add_marchspy (enquire *self, matchspy *spy);

//void
//enquire_clear_matchspies (enquire *self);

mset *
enquire_get_mset (enquire *self, unsigned int first, unsigned int maxitems);

}


#endif //CXAPIAN_ENQUIRE
