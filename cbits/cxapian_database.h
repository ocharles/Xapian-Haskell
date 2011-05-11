#ifndef CXAPIAN_DATABASE
#define CXAPIAN_DATABASE

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

/* Read-only database interface */

Xapian::Database *
database_new ();

Xapian::Database *
database_new_from_path (const char *path, const char **error);

Xapian::Database *
database_copy (Xapian::Database *other);

void
database_delete (Xapian::Database *);

void
database_add_database (Xapian::Database *, Xapian::Database *other);

void
database_reopen (Xapian::Database *);

void
database_close (Xapian::Database *);

const char *
database_get_description (Xapian::Database *);

Xapian::PostingIterator *
database_postlist_begin (Xapian::Database *, const char *tname);

Xapian::PostingIterator *
database_postlist_end (Xapian::Database *, const char *tname);

Xapian::TermIterator *
database_termlist_begin (Xapian::Database *, unsigned int docid);

Xapian::TermIterator *
database_termlist_end (Xapian::Database *, unsigned docid);

cbool
database_has_positions (Xapian::Database *);

Xapian::PositionIterator *
database_positionlist_begin (Xapian::Database *, unsigned int docid, const char *tname);

Xapian::PositionIterator *
database_positionlist_end (Xapian::Database *, unsigned int docid, const char *tname);

Xapian::TermIterator *
database_allterms_begin (Xapian::Database *);

Xapian::TermIterator *
database_allterms_end (Xapian::Database *);

Xapian::TermIterator *
database_allterms_with_prefix_begin (Xapian::Database *, const char *prefix);

Xapian::TermIterator *
database_allterms_with_prefix_end (Xapian::Database *, const char *prefix);

unsigned int
database_get_doccount (Xapian::Database *);

unsigned int
database_get_lastdocid (Xapian::Database *);

double
database_get_avlength (Xapian::Database *);

unsigned int
database_get_termfreq (Xapian::Database *, const char *tname);

cbool
database_term_exists (Xapian::Database *, const char *tname);

unsigned int
database_get_collection_freq (Xapian::Database *, const char *tname);

unsigned int
database_get_value_freq (Xapian::Database *, unsigned int valno);

const char *
database_get_value_lower_bound (Xapian::Database *, unsigned int valno);

const char *
database_get_value_upper_bound (Xapian::Database *, unsigned int valno);

unsigned int
database_get_doclength_lower_bound (Xapian::Database *);

unsigned int
database_get_doclength_upper_bound (Xapian::Database *);

unsigned int
database_get_wdf_upper_bound (Xapian::Database *, const char *term);

Xapian::ValueIterator *
database_valuestream_begin (Xapian::Database *, unsigned int valueno);

Xapian::ValueIterator *
database_valuestream_end (Xapian::Database *, unsigned int valueno);

unsigned int
database_get_doclength (Xapian::Database *, unsigned int docid);

void
database_keep_alive (Xapian::Database *);

Xapian::Document *
database_get_document (Xapian::Database *, unsigned int docid, const char **error);

const char * // max_edit_distance defaults to 2
database_get_spelling_suggestion (Xapian::Database *, const char *word,
                                  unsigned int max_edit_distance);
Xapian::TermIterator *
database_spellings_begin (Xapian::Database *);

Xapian::TermIterator *
database_spellings_end (Xapian::Database *);

Xapian::TermIterator *
database_synonyms_begin (Xapian::Database *, const char *term);

Xapian::TermIterator *
database_synonyms_end (Xapian::Database *, const char *term);

Xapian::TermIterator *
database_synonym_keys_begin (Xapian::Database *, const char *prefix);

Xapian::TermIterator *
database_synonym_keys_end (Xapian::Database *, const char *prefix);

const char *
database_get_metadata (Xapian::Database *, const char *key);

Xapian::TermIterator *
database_metadata_keys_begin (Xapian::Database *, const char *prefix);

Xapian::TermIterator *
database_metadata_keys_end (Xapian::Database *, const char *prefix);

const char *
database_get_uuid (Xapian::Database *);


/* Writable database interface */

int DB_CREATE_OR_OPEN () { return (int)Xapian::DB_CREATE_OR_OPEN; };
int DB_CREATE         () { return (int)Xapian::DB_CREATE; };
int DB_CREATE_OR_OVERWRITE () { return (int)Xapian::DB_CREATE_OR_OVERWRITE; };
int DB_OPEN           () { return (int)Xapian::DB_OPEN; };

Xapian::WritableDatabase *
database_writable_new ();

Xapian::WritableDatabase *
database_writable_new_from_path (const char *path, int action, const char **error);

Xapian::WritableDatabase *
database_writable_copy (Xapian::WritableDatabase *other);

void
database_writable_delete (Xapian::WritableDatabase *);

void
database_commit (Xapian::WritableDatabase *);

void
database_begin_transaction (Xapian::WritableDatabase *, cbool flushed);

void
database_commit_transaction (Xapian::WritableDatabase *);

void
database_cancel_transaction (Xapian::WritableDatabase *);

unsigned int
database_add_document (Xapian::WritableDatabase *, document *doc);

void
database_delete_document_by_id (Xapian::WritableDatabase *, unsigned int docid);

void
database_delete_document_by_term (Xapian::WritableDatabase *, const char *unique_term);

void
database_replace_document (Xapian::WritableDatabase *, unsigned int docid, document *doc);

void
database_add_spelling (Xapian::WritableDatabase *, const char *word, unsigned int freqinc);

void
database_remove_spelling (Xapian::WritableDatabase *, const char *word, unsigned int freqdec);

void
database_add_synonym (Xapian::WritableDatabase *, const char *term, const char *synonym);

void
database_remove_synonym (Xapian::WritableDatabase *, const char *term, const char *synonym);

void
database_clear_synonyms (Xapian::WritableDatabase *, const char *term);

void
database_set_metadata (Xapian::WritableDatabase *, const char *key, const char *value);

const char *
database_writable_get_description (Xapian::WritableDatabase *);

}

#endif //CXAPIAN_DATABASE
