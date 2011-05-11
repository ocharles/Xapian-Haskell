#ifndef CXAPIAN_DOCUMENT
#define CXAPIAN_DOCUMENT

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::Document *
document_new ();

Xapian::Document *
document_copy (Xapian::Document *);

void
document_delete (Xapian::Document *);

const char *
document_get_value (Xapian::Document *, unsigned int valueno);

void
document_add_value (Xapian::Document *, unsigned int valueno, const char *value);

void
document_remove_value(Xapian::Document *, unsigned int valueno);

void
document_clear_values (Xapian::Document *);

const char *
document_get_data (Xapian::Document *);

void
document_set_data (Xapian::Document *, const char *data);

void
document_add_posting (Xapian::Document *, const char *tname, unsigned int tpos, unsigned int wdfinc);

void
document_add_term (Xapian::Document *, const char *tname, unsigned int wdfinc);

void
document_add_boolean_term (Xapian::Document * doc, const char *term) { document_add_term (doc, term, 0); }

void
document_remove_posting (Xapian::Document *, const char *tname, unsigned int tpos, unsigned int wdfdec);

void
document_remove_term (Xapian::Document *, const char *tname);

void
document_clear_terms (Xapian::Document *);

unsigned int
document_termlist_count (Xapian::Document *);

Xapian::TermIterator *
document_termlist_begin (Xapian::Document *);

Xapian::TermIterator *
document_termlist_end (Xapian::Document *);

unsigned int
document_values_count (Xapian::Document *);

Xapian::ValueIterator *
document_values_begin (Xapian::Document *);

Xapian::ValueIterator *
document_values_end (Xapian::Document *);

unsigned int
document_get_docid (Xapian::Document *);

// document_serialise

// document_unserialise

const char *
document_get_description (Xapian::Document *);

}
#endif //CXAPIAN_DOCUMENT
