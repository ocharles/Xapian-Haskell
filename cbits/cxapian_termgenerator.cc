#include <xapian.h>
#include "cxapian_termgenerator.h"

termgenerator *
termgenerator_new ()
{
    termgenerator *tgen = new termgenerator();
    tgen->get = new Xapian::TermGenerator();
    return tgen;
}

//termgenerator_copy

void
termgenerator_delete (termgenerator *tgen)
{
    delete tgen->get;
    delete tgen;
}

void
termgenerator_set_stemmer (termgenerator *tgen, stem *stemmer)
{
    tgen->get->set_stemmer(*stemmer->get);
}

void
termgenerator_set_stopper (termgenerator *tgen, stopper *stop)
{
    tgen->get->set_stopper(stop->get);
}

void
termgenerator_set_document (termgenerator *tgen, document *doc)
{
    tgen->get->set_document(*doc->get);
}

const document *
termgenerator_get_document (termgenerator *tgen)
{
    document *doc = new document();
    doc->get = new Xapian::Document(tgen->get->get_document());
    return doc;
}

void
termgenerator_set_database (termgenerator *tgen, rwdatabase *db)
{
    tgen->get->set_database(*db->get);
}

int
termgenerator_set_flags (termgenerator *tgen, int toggle, int mask)
{
    return tgen->get->set_flags((Xapian::TermGenerator::flags) toggle,
                                (Xapian::TermGenerator::flags) mask);
}

//termgenerator_index_text_utf8

void
termgenerator_index_text (termgenerator *tgen, const char *text, unsigned int weight, const char *prefix)
{
    tgen->get->index_text(std::string(text),
                          (Xapian::termcount) weight,
                          std::string(prefix));
}

//index_text_without_positions_utf8

void
termgenerator_index_text_wo_positions (termgenerator *tgen, const char *text, unsigned int weight, const char* prefix)
{
    tgen->get->index_text_without_positions(std::string(text),
                                            (Xapian::termcount) weight,
                                            std::string(prefix));
}

void
termgenerator_increase_termpos (termgenerator *tgen, unsigned int delta)
{
    tgen->get->increase_termpos((Xapian::termcount) delta);
}

unsigned int
termgenerator_get_termpos (termgenerator *tgen)
{
    return tgen->get->get_termpos();
}

void
termgenerator_set_termpos (termgenerator *tgen, unsigned int termpos)
{
    tgen->get->set_termpos((Xapian::termcount) termpos);
}

const char *
termgenerator_get_description (termgenerator *tgen)
{
    return tgen->get->get_description().c_str();
}
