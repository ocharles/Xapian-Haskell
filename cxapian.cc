// C bindings for Xapian

#include "cxapian.h"
#include <xapian.h>

#include <stdlib.h>

struct _xapian_database {
  Xapian::Database* xapian_database;
};

xapian_database_t *
xapian_writable_db_new(const char *cFilename, int options,
                       const char **errorStr) {
  xapian_database_t *db = NULL;
  db = (xapian_database_t*) malloc(sizeof(xapian_database_t));
  if (db == NULL) {
    return NULL;
  }

  try {
    std::string filename(cFilename);
    db->xapian_database = new Xapian::WritableDatabase(filename, options);
    return db;
  }
  catch (const Xapian::Error & error) {
    *errorStr = error.get_msg().c_str();
    free(db);
    return NULL;
  }
}

void xapian_writable_db_add_document(xapian_database_t *database,
                                     void *vdocument) {
  Xapian::Document *document = (Xapian::Document*)vdocument;
  Xapian::WritableDatabase *wdb =
    static_cast <Xapian::WritableDatabase *> (database->xapian_database);

  wdb->add_document(*document);
  wdb->flush();
}

void* xapian_document_new() {
  return new Xapian::Document();
}

void xapian_document_set_data (void *doc, const char* data)
{
  Xapian::Document *document = (Xapian::Document*)doc;
  document->set_data(std::string(data));
}

void xapian_document_add_posting (void *doc, const char* posting, int pos)
{
  Xapian::Document *document = (Xapian::Document*)doc;
  document->add_posting(std::string(posting), pos);
}

xapian_database_t *
xapian_database_new (const char *cFilename, const char **errorStr) {
  xapian_database_t *db = NULL;
  db = (xapian_database_t *) malloc(sizeof(xapian_database_t));
  if (db == NULL) {
    return NULL;
  }

  try {
    std::string filename(cFilename);
    db->xapian_database = new Xapian::Database(filename);
    return db;
  }
  catch (const Xapian::Error & error) {
    *errorStr = error.get_msg().c_str();
    free(db);
    return NULL;
  }
}

void *xapian_enquire_new (xapian_database_t *database) {
  return new Xapian::Enquire(*(database->xapian_database));
}

void *xapian_query_new (const char* term) {
  return new Xapian::Query(std::string(term));
}

void *xapian_query_combine (int op, void *vqa, void *vqb) {
  Xapian::Query *queryA = (Xapian::Query*)vqa;
  Xapian::Query *queryB = (Xapian::Query*)vqb;
  return new Xapian::Query((Xapian::Query::op) op, *queryA, *queryB);
}

const char *xapian_query_describe (void *vqa) {
  Xapian::Query *queryA = (Xapian::Query*)vqa;
  return queryA->get_description().c_str();
}
