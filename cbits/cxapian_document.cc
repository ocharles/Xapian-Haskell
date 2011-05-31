#include <xapian.h>
#include "cxapian_document.h"

Xapian::Document *
document_new ()
{
    return new Xapian::Document();
}

Xapian::Document *
document_copy (Xapian::Document *original)
{
    return new Xapian::Document(*original);
}

void
document_delete (Xapian::Document *doc)
{
    delete doc;
}

const char *
document_get_value (Xapian::Document *doc, unsigned int valueno)
{
    return doc->get_value((Xapian::valueno) valueno).c_str();
}

void
document_add_value (Xapian::Document *doc, unsigned int valueno, const char *value)
{
    doc->add_value((Xapian::valueno)valueno, std::string(value));
}

void
document_remove_value(Xapian::Document *doc, unsigned int valueno)
{
    doc->remove_value((Xapian::valueno) valueno);
}

void
document_clear_values (Xapian::Document *doc)
{
    doc->clear_values();
}

std::string *
document_get_data (Xapian::Document *doc)
{
    std::string *str = new std::string(doc->get_data());
    return str;
}

void
document_set_data (Xapian::Document *doc, std::string *data)
{
    doc->set_data(*data);
}

void
document_add_posting (Xapian::Document *doc, const char *tname, unsigned int tpos, unsigned int wdfinc)
{
    doc->add_posting(std::string(tname), (Xapian::termpos)tpos, (Xapian::termcount)wdfinc);
}

void
document_add_term (Xapian::Document *doc, const char *tname, unsigned int wdfinc)
{
    doc->add_term(std::string(tname), (Xapian::termcount)wdfinc);
}

void
document_remove_posting (Xapian::Document *doc, const char *tname, unsigned int tpos, unsigned int wdfdec)
{
    doc->remove_posting(std::string(tname), (Xapian::termpos)tpos, (Xapian::termcount)wdfdec);
}

void
document_remove_term (Xapian::Document *doc, const char *tname)
{
    doc->remove_term(std::string(tname));
}

void
document_clear_terms (Xapian::Document *doc)
{
    doc->clear_terms();
}

unsigned int
document_termlist_count (Xapian::Document *doc)
{
    return doc->termlist_count();
}

Xapian::TermIterator *
document_termlist_begin (Xapian::Document *doc)
{
    return new Xapian::TermIterator(doc->termlist_begin());
}

Xapian::TermIterator *
document_termlist_end (Xapian::Document *doc)
{
    return new Xapian::TermIterator(doc->termlist_end());
}

unsigned int
document_values_count (Xapian::Document *doc)
{
    return doc->values_count();
}

Xapian::ValueIterator *
document_values_begin (Xapian::Document *doc)
{
    return new Xapian::ValueIterator(doc->values_begin());
}

Xapian::ValueIterator *
document_values_end (Xapian::Document *doc)
{
    return new Xapian::ValueIterator(doc->values_end());
}

unsigned int
document_get_docid (Xapian::Document *doc)
{
    return doc->get_docid();
}

// document_serialise

// document_unserialise

const char *
document_get_description (Xapian::Document *doc)
{
    return doc->get_description().c_str();
}
