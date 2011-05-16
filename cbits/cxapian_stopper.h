#ifndef CXAPIAN_STOPPER
#define CXAPIAN_STOPPER

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::Stopper *
stopper_simple_stopper_new ();

//Xapian::Stopper *
//stopper_new_simple_stopper_from_iterator

void
stopper_simple_stopper_add (Xapian::Stopper *, const char *word);

void
stopper_delete (Xapian::Stopper *);

cbool
stopper_check (Xapian::Stopper *, const char *term); // operator()

const char *
stopper_get_description (Xapian::Stopper *);

}

#endif //CXAPIAN_STOPPER
