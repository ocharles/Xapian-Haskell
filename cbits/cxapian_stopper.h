#ifndef CXAPIAN_STOPPER
#define CXAPIAN_STOPPER

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

stopper *
stopper_simple_stopper_new ();

//stopper *
//stopper_new_simple_stopper_from_iterator

void
stopper_simple_stopper_add (stopper *, const char *word);

void
stopper_delete (stopper *);

cbool
stopper_check (stopper *, const char *term); // operator()

const char *
stopper_get_description (stopper *);

}

#endif //CXAPIAN_STOPPER
