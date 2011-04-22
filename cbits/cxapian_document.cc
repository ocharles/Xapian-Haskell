#include <xapian.h>
#include "cxapian_document.h"

document *
document_new ()
{
    document *doc = new document();
    doc->get = new Xapian::Document();
    return doc;
}

document *
document_copy (document *original)
{
    document *doc = new document();
    doc->get = new Xapian::Document(*original->get);
    return doc;
}

void
document_delete (document *doc)
{
    delete doc->get;
    delete doc;
}

const char *
document_get_value (document *doc, unsigned int valueno)
{
    return doc->get->get_value((Xapian::valueno) valueno).c_str();
}

void
document_add_value (document *doc, unsigned int valueno, const char *value)
{
    doc->get->add_value((Xapian::valueno)valueno, std::string(value));
}

void
document_remove_value(document *doc, unsigned int valueno)
{
    doc->get->remove_value((Xapian::valueno) valueno);
}

void
document_clear_values (document *doc)
{
    doc->get->clear_values();
}

const char *
document_get_data (document *doc)
{
    return doc->get->get_data().c_str();
}

void
document_set_data (document *doc, const char *data)
{
    doc->get->set_data(std::string(data));
}

void
document_add_posting (document *doc, const char *tname, unsigned int tpos, unsigned int wdfinc)
{
    doc->get->add_posting(std::string(tname), (Xapian::termpos)tpos, (Xapian::termcount)wdfinc);
}

void
document_add_term (document *doc, const char *tname, unsigned int wdfinc)
{
    doc->get->add_term(std::string(tname), (Xapian::termcount)wdfinc);
}

void
document_remove_posting (document *doc, const char *tname, unsigned int tpos, unsigned int wdfdec)
{
    doc->get->remove_posting(std::string(tname), (Xapian::termpos)tpos, (Xapian::termcount)wdfdec);
}

void
document_remove_term (document *doc, const char *tname)
{
    doc->get->remove_term(std::string(tname));
}

void
document_clear_terms (document *doc)
{
    doc->get->clear_terms();
}

unsigned int
document_termlist_count (document *doc)
{
    return doc->get->termlist_count();
}

termiterator *
document_termlist_begin (document *doc)
{
    termiterator *termit = new termiterator();
    *termit->iter = doc->get->termlist_begin();
    return termit;
}

termiterator *
document_termlist_end (document *doc)
{
    termiterator *termit = new termiterator();
    *termit->iter = doc->get->termlist_end();
    return termit;
}

unsigned int
document_values_count (document *doc)
{
    return doc->get->values_count();
}

valueiterator *
document_values_begin (document *doc)
{
    valueiterator *valit = new valueiterator();
    *valit->iter = doc->get->values_begin();
    return valit;
}

valueiterator *
document_values_end (document *doc)
{
    valueiterator *valit = new valueiterator();
    *valit->iter = doc->get->values_end();
    return valit;
}

unsigned int
document_get_docid (document *doc)
{
    return doc->get->get_docid();
}

// document_serialise

// document_unserialise

const char *
document_get_description (document *doc)
{
    return doc->get->get_description().c_str();
}
