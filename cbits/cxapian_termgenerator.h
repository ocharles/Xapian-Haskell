#ifndef CXAPIAN_TERMGENERATOR
#define CXAPIAN_TERMGENERATOR

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

termgenerator *
termgenerator_new ();

//termgenerator_copy

void
termgenerator_delete (termgenerator *tgen);

void
termgenerator_set_stemmer (termgenerator *tgen, stem *stemmer);

void
termgenerator_set_stopper (termgenerator *tgen, stopper *stop);

void
termgenerator_set_document (termgenerator *tgen, document *doc);

const document *
termgenerator_get_document (termgenerator *tgen);

void
termgenerator_set_database (termgenerator *tgen, rwdatabase *db);

int
termgenerator_set_flags (termgenerator *tgen, int toggle, int mask);

//termgenerator_index_text_utf8

void
termgenerator_index_text (termgenerator *tgen, const char *text, unsigned int weight, const char *prefix);

//index_text_without_positions_utf8

void
termgenerator_index_text_wo_positions (termgenerator *tgen, const char *text, unsigned int weight, const char* prefix);

void
termgenerator_increase_termpos (termgenerator *tgen, unsigned int delta);

unsigned int
termgenerator_get_termpos (termgenerator *tgen);

void
termgenerator_set_termpos (termgenerator *tgen, unsigned int termpos);

const char *
termgenerator_get_description (termgenerator *tgen);

}

#endif //CXAPIAN_TERMGENERATOR
