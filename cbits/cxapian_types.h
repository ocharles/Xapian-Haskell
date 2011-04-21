#ifndef CXAPIAN_TYPES
#define CXAPIAN_TYPES

struct _termgenerator {
    Xapian::TermGenerator *get;
};

struct _stem {
    Xapian::Stem *get;
};

struct _stopper {
    Xapian::Stopper *get;
};

struct _document {
    Xapian::Document *get;
};

struct _rwdatabase {
    Xapian::WritableDatabase *get;
};


typedef struct _termgenerator termgenerator;
typedef struct _stem stem;
typedef struct _stopper stopper;
typedef struct _document document;
typedef struct _rwdatabase rwdatabase;

#endif //CXAPIAN_TYPES
