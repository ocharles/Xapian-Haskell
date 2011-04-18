// C bindings for Xapian

#include "cxapian.h"
#include <xapian.h>

struct _xapian_database {
  Xapian::Database* xapian_database;
};

struct _xapian_document {
  Xapian::Document xapian_document;
};

struct _xapian_enquire {
  Xapian::Enquire* xapian_enquire;
};

struct _xapian_query {
  Xapian::Query xapian_query;
};

struct _xapian_msets {
  Xapian::MSetIterator iterator;
  Xapian::MSetIterator end;
};

struct _xapian_terms {
  Xapian::TermIterator iterator;
  Xapian::TermIterator end;
};

struct _xapian_stem {
  Xapian::Stem *xapian_stem;
};

xapian_database_t *
xapian_writable_db_new(const char *cFilename, int options,
                       const char **errorStr) {
  xapian_database_t *db = new xapian_database_t();

  try {
    std::string filename(cFilename);
    db->xapian_database = new Xapian::WritableDatabase(filename, options);
    return db;
  }
  catch (const Xapian::Error & error) {
    *errorStr = error.get_msg().c_str();
    delete db;
    return NULL;
  }
}

unsigned int
xapian_writable_db_add_document(xapian_database_t *database,
                                xapian_document_t *document) {
  Xapian::WritableDatabase *wdb =
    static_cast <Xapian::WritableDatabase *> (database->xapian_database);
  return wdb->add_document(document->xapian_document);
}

xapian_database_t *
xapian_database_new (const char *cFilename, const char **errorStr) {
  xapian_database_t *db = new xapian_database_t;

  try {
    std::string filename(cFilename);
    db->xapian_database = new Xapian::Database(filename);
    return db;
  }
  catch (const Xapian::Error & error) {
    *errorStr = error.get_msg().c_str();
    delete db;
    return NULL;
  }
}

void
xapian_database_delete (xapian_database_t *database) {
  delete database->xapian_database;
  delete database;
}

xapian_document_t *
xapian_document_new() {
  xapian_document_t *document = new xapian_document_t;

  document->xapian_document = *(new Xapian::Document());
  return document;
}

xapian_document_t *
xapian_get_document (xapian_database_t *database, int doc_id, const char **errorStr)
{
  xapian_document_t *document = new xapian_document_t;

  try {
    document->xapian_document = database->xapian_database->get_document(doc_id);
    return document;
  }
  catch (const Xapian::DocNotFoundError & error) {
    *errorStr = error.get_msg().c_str();
    delete document;
    return NULL;
  }
}

void
xapian_document_delete(xapian_document_t *document) {
  delete document;
}

void
xapian_document_set_data (xapian_document_t *doc, const char* data)
{
  doc->xapian_document.set_data(std::string(data));
}

const char *
xapian_document_get_data (xapian_document_t *document)
{
  return document->xapian_document.get_data().c_str();
}

void
xapian_document_add_posting (xapian_document_t *doc, const char* posting, int pos)
{
  doc->xapian_document.add_posting(std::string(posting), pos);
}

xapian_enquire_t *
xapian_enquire_new (xapian_database_t *database) {
  xapian_enquire_t *enquire = new xapian_enquire_t;

  enquire->xapian_enquire = new Xapian::Enquire(*(database->xapian_database));
  return enquire;
}

void
xapian_enquire_delete(xapian_enquire_t *enquire) {
  delete enquire->xapian_enquire;
  delete enquire;
}

xapian_query_t *
xapian_query_empty () {
  xapian_query_t *query = new xapian_query_t;

  query->xapian_query = Xapian::Query();
  return query;
}

xapian_query_t *
xapian_query_new (const char* term) {
  xapian_query_t *query = new xapian_query_t;

  query->xapian_query = Xapian::Query(std::string(term));
  return query;
}

xapian_query_t *
xapian_query_combine (int op, xapian_query_t *qa, xapian_query_t *qb) {
  xapian_query_t *query = new xapian_query_t;

  query->xapian_query = Xapian::Query((Xapian::Query::op) op,
                                      qa->xapian_query,
                                      qb->xapian_query);
  return query;
}

xapian_query_t *
xapian_query_new_value (int op, int valno, const char *value) {
  xapian_query_t *query = new xapian_query_t;

  query->xapian_query = Xapian::Query((Xapian::Query::op) op,
                                      (Xapian::valueno) valno,
                                      std::string(value));
  return query;
}

xapian_query_t *
xapian_query_new_double (int op, xapian_query_t *q, double d) {
  xapian_query_t *query = new xapian_query_t;

  query->xapian_query = Xapian::Query((Xapian::Query::op) op,
                                      q->xapian_query,
                                      d);
  return query;
}

const char *
xapian_query_describe (xapian_query_t *query) {
  return query->xapian_query.get_description().c_str();
}

void
xapian_query_delete(xapian_query_t *query) {
  delete query;
}

xapian_msets_t *
xapian_enquire_query (xapian_enquire_t* enquire, xapian_query_t *query,
                      int min, int max) {
  xapian_msets_t *msets = new xapian_msets_t;

  enquire->xapian_enquire->set_query(query->xapian_query);
  Xapian::MSet xapian_msets = enquire->xapian_enquire->get_mset(min, max);

  msets->iterator = xapian_msets.begin();
  msets->end = xapian_msets.end();

  return msets;
}

int
xapian_msets_valid (xapian_msets_t *msets) {
  return msets->iterator != msets->end;
}

int
xapian_msets_get (xapian_msets_t *msets) {
  return *msets->iterator;
}

void
xapian_msets_next(xapian_msets_t *msets) {
  msets->iterator++;
}

xapian_terms_t *
xapian_get_terms (_xapian_document* document) {
  xapian_terms_t *terms = new xapian_terms_t;

  terms->iterator = document->xapian_document.termlist_begin();
  terms->end = document->xapian_document.termlist_end();

  return terms;
}

int
xapian_terms_valid (xapian_terms_t *terms) {
  return terms->iterator != terms->end;
}

const char *
xapian_terms_get (xapian_terms_t *terms) {
  return (*terms->iterator).c_str();
}

void
xapian_terms_next(xapian_terms_t *terms) {
  terms->iterator++;
}

xapian_stem_t *
xapian_stem_new(const char *language) {
  xapian_stem_t *stem = new xapian_stem_t;
  stem->xapian_stem = new Xapian::Stem(std::string(language));
  return stem;
}

void
xapian_stem_delete (xapian_stem_t *stem) {
  delete stem->xapian_stem;
  delete stem;
}

void
xapian_stem_string (xapian_stem_t *stem, xapian_document_t *document,
                    const char *string) {
  Xapian::TermGenerator tg;
  tg.set_stemmer(*stem->xapian_stem);
  tg.set_document(document->xapian_document);
  tg.index_text(std::string(string));
}

xapian_query_t *
xapian_parse_query (const char *query_string,
                    xapian_stem_t *stem) {
  xapian_query_t *query = new xapian_query_t;
  Xapian::QueryParser query_parser;
  query_parser.set_stemmer(*stem->xapian_stem);
  query->xapian_query = query_parser.parse_query(std::string(query_string),
                                                 0);

  return query;
}
