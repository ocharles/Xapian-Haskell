#ifndef XAPIAN_H
#define XAPIAN_H

typedef struct _xapian_database xapian_database_t;
typedef struct _xapian_document xapian_document_t;
typedef struct _xapian_enquire xapian_enquire_t;

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

  void *xapian_query_new (const char* term);

  void *xapian_query_combine (int op, void *vqa, void *vqb);

  const char *xapian_query_describe (void *vqa);
}

#endif
