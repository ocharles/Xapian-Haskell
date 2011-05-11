#include <xapian.h>
#include "cxapian_enquire.h"

Xapian::Enquire *
enquire_new (Xapian::Database *db)
{
    return new Xapian::Enquire(*db);
}

void
enquire_delete (Xapian::Enquire *self)
{
    delete self;
}

void
enquire_set_query (Xapian::Enquire *self, Xapian::Query *query, unsigned int qlen)
{
    self->set_query(*query, qlen);
}

//query *
//enquire_get_query (Xapian::Enquire *self)

//void
//enquire_add_marchspy (Xapian::Enquire *self, matchspy *spy);

//void
//enquire_clear_matchspies (Xapian::Enquire *self);

Xapian::MSet *
enquire_get_mset (Xapian::Enquire *self, unsigned int first, unsigned int maxitems)
{
    return new Xapian::MSet( self->get_mset(first, maxitems) );
}
