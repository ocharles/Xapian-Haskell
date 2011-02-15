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

}

#endif
