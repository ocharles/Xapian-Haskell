#ifndef CXAPIAN_TERMGENERATOR
#define CXAPIAN_TERMGENERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::TermGenerator *
termgenerator_new ();

//termgenerator_copy

void
termgenerator_delete (Xapian::TermGenerator *self);

void
termgenerator_set_stemmer (Xapian::TermGenerator *self, Xapian::Stem *stemmer);

void
termgenerator_set_stopper (Xapian::TermGenerator *self, Xapian::Stopper *stop);

void
termgenerator_set_document (Xapian::TermGenerator *self, Xapian::Document *doc);

const Xapian::Document *
termgenerator_get_document (Xapian::TermGenerator *self);

void
termgenerator_set_database (Xapian::TermGenerator *self, Xapian::WritableDatabase *db);

int
termgenerator_set_flags (Xapian::TermGenerator *self, int toggle, int mask);

//termgenerator_index_text_utf8

void
termgenerator_index_text (Xapian::TermGenerator *self, const char *text, unsigned int weight, const char *prefix);

//index_text_without_positions_utf8

void
termgenerator_index_text_wo_positions (Xapian::TermGenerator *self, const char *text, unsigned int weight, const char* prefix);

void
termgenerator_increase_termpos (Xapian::TermGenerator *self, unsigned int delta);

unsigned int
termgenerator_get_termpos (Xapian::TermGenerator *self);

void
termgenerator_set_termpos (Xapian::TermGenerator *self, unsigned int termpos);

const char *
termgenerator_get_description (Xapian::TermGenerator *self);

}

#endif //CXAPIAN_TERMGENERATOR
