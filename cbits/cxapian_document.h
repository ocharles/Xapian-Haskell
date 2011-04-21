#ifndef CXAPIAN_DOCUMENT
#define CXAPIAN_DOCUMENT

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

document *
document_new ();

document *
document_copy (document *);

void
document_delete (document *);

const char *
document_get_value (document *, unsigned int valueno);

void
document_add_value (document *, unsigned int valueno, const char *value);

void
document_remove_value(document *, unsigned int valueno);

void
document_clear_values (document *);

const char *
document_get_data (document *);

void
document_set_data (document *, const char *data);

void
document_add_posting (document *, const char *tname, unsigned int tpos, unsigned int wdfinc);

void
document_add_term (document *, const char *tname, unsigned int wdfinc);

void
document_add_boolean_term (document * doc, const char *term) { document_add_term (doc, term, 0); }

void
document_remove_posting (document *, const char *tname, unsigned int tpos, unsigned int wdfdec);

void
document_remove_term (document *, const char *tname);

void
document_clear_terms (document *);

unsigned int
document_termlist_count (document *);

termiterator *
document_termlist_iterator (document *);

unsigned int
document_values_count (document *);

valueiterator *
document_values_iterator (document *);

unsigned int
document_get_docid (document *);

// document_serialise

// document_unserialise

const char *
document_get_description (document *);

}
#endif //CXAPIAN_DOCUMENT
