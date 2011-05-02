#include <xapian.h>
#include "cxapian_stopper.h"


stopper *
stopper_simple_stopper_new ()
{
    stopper *s = new stopper();
    s->get = new Xapian::SimpleStopper();
    return s;
}

//stopper *
//stopper_new_simple_stopper_from_iterator

void // make sure you call this on a Xapian::SimpleStopper
stopper_simple_stopper_add (stopper *s, const char *word)
{
    ((Xapian::SimpleStopper*)s->get)->add(std::string(word));
}

void
stopper_delete (stopper *s)
{
    delete s->get;
    delete s;
}

cbool
stopper_check (stopper *s, const char *term)
{
    return (*s->get)(std::string(term));
}

const char *
stopper_get_description (stopper *s)
{
    return s->get->get_description().c_str();
}
