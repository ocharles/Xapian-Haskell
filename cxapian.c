// C bindings for Xapian

#include "cxapian.h"
#include <xapian.h>

void* xapian_writable_db_new(const char *cFilename, int options,
                             const char **errorStr) {
  using std::string;
  using Xapian::WritableDatabase;

  string filename(cFilename);
  try {
    WritableDatabase *database = new WritableDatabase(filename, options);
    return database;
  }
  catch (const Xapian::Error & error) {
    *errorStr = error.get_msg().c_str();
    return NULL;
  }
}

extern void xapian_writable_db_add_document(void *vdatabase, void *vdocument) {
  Xapian::Document *document = (Xapian::Document*)vdocument;
  Xapian::WritableDatabase *database = (Xapian::WritableDatabase*)vdatabase;
  database->add_document(*document);
  database->flush();
}

void* xapian_document_new() {
  return new Xapian::Document();
}

extern void xapian_document_set_data (void *doc, const char* data)
{
  using std::string;
  Xapian::Document *document = (Xapian::Document*)doc;
  document->set_data(string(data));
}

extern void xapian_document_add_posting (void *doc, const char* posting, int pos)
{
  using std::string;
  Xapian::Document *document = (Xapian::Document*)doc;
  document->add_posting(string(posting), pos);
}
