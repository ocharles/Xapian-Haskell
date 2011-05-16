#include <xapian.h>
#include "cxapian_stopper.h"


Xapian::Stopper *
stopper_simple_stopper_new ()
{
    return new Xapian::SimpleStopper();
}

//Xapian::Stopper *
//stopper_new_simple_stopper_from_iterator

void // make sure you call this on a Xapian::SimpleStopper
stopper_simple_stopper_add (Xapian::Stopper *s, const char *word)
{
    ((Xapian::SimpleStopper*)s)->add(std::string(word));
}

void
stopper_delete (Xapian::Stopper *s)
{
    delete s;
}

cbool
stopper_check (Xapian::Stopper *s, const char *term)
{
    return (*s)(std::string(term));
}

const char *
stopper_get_description (Xapian::Stopper *s)
{
    return s->get_description().c_str();
}
