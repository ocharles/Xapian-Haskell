#include <xapian.h>
#include "cxapian_termgenerator.h"

Xapian::TermGenerator *
termgenerator_new ()
{
    return new Xapian::TermGenerator();
}

//termgenerator_copy

void
termgenerator_delete (Xapian::TermGenerator *self)
{
    delete self;
}

void
termgenerator_set_stemmer (Xapian::TermGenerator *self, Xapian::Stem *stemmer)
{
    self->set_stemmer(*stemmer);
}

void
termgenerator_set_stopper (Xapian::TermGenerator *self, Xapian::Stopper *stop)
{
    self->set_stopper(stop);
}

void
termgenerator_set_document (Xapian::TermGenerator *self, Xapian::Document *doc)
{
    self->set_document(*doc);
}

const Xapian::Document *
termgenerator_get_document (Xapian::TermGenerator *self)
{
    return new Xapian::Document(self->get_document());
}

void
termgenerator_set_database (Xapian::TermGenerator *self, Xapian::WritableDatabase *db)
{
    self->set_database(*db);
}

int
termgenerator_set_flags (Xapian::TermGenerator *self, int toggle, int mask)
{
    return self->set_flags((Xapian::TermGenerator::flags) toggle,
                           (Xapian::TermGenerator::flags) mask);
}

//termgenerator_index_text_utf8

void
termgenerator_index_text (Xapian::TermGenerator *self, const char *text, unsigned int weight, const char *prefix)
{
    self->index_text(std::string(text),
                     (Xapian::termcount) weight,
                     std::string(prefix));
}

//index_text_without_positions_utf8

void
termgenerator_index_text_wo_positions (Xapian::TermGenerator *self, const char *text, unsigned int weight, const char* prefix)
{
    self->index_text_without_positions(std::string(text),
                                       (Xapian::termcount) weight,
                                       std::string(prefix));
}

void
termgenerator_increase_termpos (Xapian::TermGenerator *self, unsigned int delta)
{
    self->increase_termpos((Xapian::termcount) delta);
}

unsigned int
termgenerator_get_termpos (Xapian::TermGenerator *self)
{
    return self->get_termpos();
}

void
termgenerator_set_termpos (Xapian::TermGenerator *self, unsigned int termpos)
{
    self->set_termpos((Xapian::termcount) termpos);
}

const char *
termgenerator_get_description (Xapian::TermGenerator *self)
{
    return self->get_description().c_str();
}
