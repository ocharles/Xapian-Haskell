// C bindings for Xapian

#include "cxapian.h"
#include <xapian.h>

#include <stdlib.h>

struct _xapian_database {
  Xapian::Database* xapian_database;
};

struct _xapian_document {
  Xapian::Document* xapian_document;
};

struct _xapian_enquire {
  Xapian::Enquire* xapian_enquire;
};

struct _xapian_query {
  Xapian::Query* xapian_query;
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

void
xapian_writable_db_add_document(xapian_database_t *database,
                                xapian_document_t *document) {
  Xapian::WritableDatabase *wdb =
    static_cast <Xapian::WritableDatabase *> (database->xapian_database);

  wdb->add_document(*(document->xapian_document));
  wdb->flush();
}

xapian_document_t *
xapian_document_new() {
  xapian_document_t *document = NULL;
  document = (xapian_document_t *)malloc(sizeof(xapian_document_t));
  if (document == NULL) {
    return NULL;
  }

  document->xapian_document = new Xapian::Document();
  return document;
}

void
xapian_document_set_data (xapian_document_t *doc, const char* data)
{
  doc->xapian_document->set_data(std::string(data));
}

void
xapian_document_add_posting (xapian_document_t *doc, const char* posting, int pos)
{
  doc->xapian_document->add_posting(std::string(posting), pos);
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

xapian_enquire_t *
xapian_enquire_new (xapian_database_t *database) {
  xapian_enquire_t *enquire = NULL;
  enquire = (xapian_enquire_t *)malloc(sizeof(xapian_enquire_t));
  if(enquire == NULL) {
    return NULL;
  }

  enquire->xapian_enquire = new Xapian::Enquire(*(database->xapian_database));
  return enquire;
}

xapian_query_t *
xapian_query_new (const char* term) {
  xapian_query_t *query = NULL;
  query = (xapian_query_t *)malloc(sizeof(xapian_query_t));
  if(query == NULL) {
    return NULL;
  }

  query->xapian_query = new Xapian::Query(std::string(term));
  return query;
}

xapian_query_t *
xapian_query_combine (int op, xapian_query_t *qa, xapian_query_t *qb) {
  xapian_query_t *query = NULL;
  query = (xapian_query_t *)malloc(sizeof(xapian_query_t));
  if(query == NULL) {
    return NULL;
  }

  query->xapian_query = new Xapian::Query((Xapian::Query::op) op,
                                          *(qa->xapian_query),
                                          *(qb->xapian_query));
  return query;
}

const char *
xapian_query_describe (xapian_query_t *query) {
  return query->xapian_query->get_description().c_str();
}
