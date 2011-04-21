#ifndef CXAPIAN_TYPES
#define CXAPIAN_TYPES

// cxapian_termgenerator.cc
struct _termgenerator { Xapian::TermGenerator *get; };
typedef struct _termgenerator termgenerator;

// cxapian_stem.cc
struct _stem { Xapian::Stem *get; };
typedef struct _stem stem;

struct _stopper { Xapian::Stopper *get; };
typedef struct _stopper stopper;

// cxapian_document.cc
struct _document { Xapian::Document *get; };
typedef struct _document document;

struct _rwdatabase { Xapian::WritableDatabase *get; };
typedef struct _rwdatabase rwdatabase;

// cxapian_query.cc
struct _query { Xapian::Query *get; };
typedef struct _query query;

struct _termiterator { Xapian::TermIterator *iter; };
typedef struct _termiterator termiterator;

struct _valueiterator { Xapian::ValueIterator *iter; };
typedef struct _valueiterator valueiterator;

#endif //CXAPIAN_TYPES
