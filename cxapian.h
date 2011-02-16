#ifndef XAPIAN_H
#define XAPIAN_H

extern "C" {

  extern void* xapian_writable_db_new (const char *filename, int action,
                                       const char **error);

  extern void xapian_writable_db_add_document(void *database, void *document);

  extern void* xapian_document_new ();

  extern void xapian_document_set_data (void *document, const char* data);

  extern void xapian_document_add_posting (void *doc, const char* posting,
                                           int pos);

  extern void* xapian_database_new (const char *cFilename, const char **errorStr);

  extern void *xapian_enquire_new (void *database);

  extern void *xapian_query_new (const char* term);

  extern void *xapian_query_combine (int op, void *vqa, void *vqb);
}

#endif
