#ifndef CXAPIAN_TYPES
#define CXAPIAN_TYPES

// cxapian_database.cc
struct _database { Xapian::Database *get; };
typedef struct _database database;

// cxapian_termgenerator.cc
struct _termgenerator { Xapian::TermGenerator *get; };
typedef struct _termgenerator termgenerator;

// cxapian_stem.cc
struct _stem { Xapian::Stem *get; };
typedef struct _stem stem;

// cxapian_stopper.cc
struct _stopper { Xapian::Stopper *get; };
typedef struct _stopper stopper;

// cxapian_document.cc
struct _document { Xapian::Document *get; };
typedef struct _document document;

// cxapian_query.cc
struct _query { Xapian::Query *get; };
typedef struct _query query;

// cxapian_mset.cc
struct _mset { Xapian::MSet *get; };
typedef struct _mset mset;

// cxapian_termiterator.cc
struct _termiterator { Xapian::TermIterator *iter; };
typedef struct _termiterator termiterator;

// cxapian_valueiterator.cc
struct _valueiterator { Xapian::ValueIterator *iter; };
typedef struct _valueiterator valueiterator;

// cxapian_positioniterator.cc
struct _positioniterator { Xapian::PositionIterator *iter; };
typedef struct _positioniterator positioniterator;

// TODO
struct _postingiterator { Xapian::PostingIterator *iter; };
typedef struct _postingiterator postingiterator;

// cxapian_msetiterator.cc
struct _msetiterator { Xapian::MSetIterator *iter; };
typedef struct _msetiterator msetiterator;

#endif //CXAPIAN_TYPES
