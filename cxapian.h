#ifndef XAPIAN_H
#define XAPIAN_H

typedef struct _xapian_database xapian_database_t;
typedef struct _xapian_document xapian_document_t;
typedef struct _xapian_enquire xapian_enquire_t;
typedef struct _xapian_query xapian_query_t;

extern "C" {

  xapian_database_t *
  xapian_writable_db_new (const char *filename, int action,
                          const char **error);

  void
  xapian_writable_db_add_document(xapian_database_t *database,
                                  xapian_document_t *document);

  xapian_document_t *
  xapian_document_new ();

  void
  xapian_document_set_data (xapian_document_t *document, const char* data);

  void
  xapian_document_add_posting (xapian_document_t *doc, const char* posting,
                                    int pos);

  xapian_database_t *
  xapian_database_new (const char *cFilename, const char **errorStr);

  xapian_enquire_t *
  xapian_enquire_new (xapian_database_t *database);

  xapian_query_t *
  xapian_query_new (const char* term);

  xapian_query_t *
  xapian_query_combine (int op, xapian_query_t *vqa, xapian_query_t *vqb);

  const char *
  xapian_query_describe (xapian_query_t *vqa);
}

#endif
