#ifndef CXAPIAN_TERMGENERATOR
#define CXAPIAN_TERMGENERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

Xapian::TermGenerator *
termgenerator_new ();

//termgenerator_copy

void
termgenerator_delete (Xapian::TermGenerator *tgen);

void
termgenerator_set_stemmer (Xapian::TermGenerator *tgen, stem *stemmer);

void
termgenerator_set_stopper (Xapian::TermGenerator *tgen, stopper *stop);

void
termgenerator_set_document (Xapian::TermGenerator *tgen, document *doc);

const Xapian::Document *
termgenerator_get_document (Xapian::TermGenerator *tgen);

void
termgenerator_set_database (Xapian::TermGenerator *tgen, Xapian::WritableDatabase *db);

int
termgenerator_set_flags (Xapian::TermGenerator *tgen, int toggle, int mask);

//termgenerator_index_text_utf8

void
termgenerator_index_text (Xapian::TermGenerator *tgen, const char *text, unsigned int weight, const char *prefix);

//index_text_without_positions_utf8

void
termgenerator_index_text_wo_positions (Xapian::TermGenerator *tgen, const char *text, unsigned int weight, const char* prefix);

void
termgenerator_increase_termpos (Xapian::TermGenerator *tgen, unsigned int delta);

unsigned int
termgenerator_get_termpos (Xapian::TermGenerator *tgen);

void
termgenerator_set_termpos (Xapian::TermGenerator *tgen, unsigned int termpos);

const char *
termgenerator_get_description (Xapian::TermGenerator *tgen);

}

#endif //CXAPIAN_TERMGENERATOR
