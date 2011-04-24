#include <xapian.h>
#include "cxapian_enquire.h"

enquire *
enquire_new (database *db)
{
    enquire *self = new enquire();
    self->get = new Xapian::Enquire(*db->get);
    return self;
}

void
enquire_set_query (enquire *self, query *query, unsigned int qlen)
{
    self->get->set_query(*query->get, qlen);
}

//query *
//enquire_get_query (enquire *self)

//void
//enquire_add_marchspy (enquire *self, matchspy *spy);

//void
//enquire_clear_matchspies (enquire *self);

mset *
enquire_get_mset (enquire *self, unsigned int first, unsigned int maxitems)
{
    mset *m = new mset();
    m->get = new Xapian::MSet( self->get->get_mset(first, maxitems) );
    return m;
}
