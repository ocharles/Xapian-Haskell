#ifndef CXAPIAN_DATABASE
#define CXAPIAN_DATABASE

#include <xapian.h>
#include "cxapian_types.h"

extern "C" {

/* Read-only database interface */

database *
database_new ();

database *
database_new_from_path (const char *path, const char **error);

database *
database_copy (database *other);

void
database_delete (database *);

void
database_add_database (database *, database *other);

void
database_reopen (database *);

void
database_close (database *);

const char *
database_get_description (database *);

postingiterator *
database_postlist_begin (database *, const char *tname);

postingiterator *
database_postlist_end (database *, const char *tname);

termiterator *
database_termlist_begin (database *, unsigned int docid);

termiterator *
database_termlist_end (database *, unsigned docid);

bool
database_has_positions (database *);

positioniterator *
database_positionlist_begin (database *, unsigned int docid, const char *tname);

positioniterator *
database_positionlist_end (database *, unsigned int docid, const char *tname);

termiterator *
database_allterms_begin (database *);

termiterator *
database_allterms_end (database *);

termiterator *
database_allterms_with_prefix_begin (database *, const char *prefix);

termiterator *
database_allterms_with_prefix_end (database *, const char *prefix);

unsigned int
database_get_doccount (database *);

unsigned int
database_get_lastdocid (database *);

double
database_get_avlength (database *);

unsigned int
database_get_termfreq (database *, const char *tname);

bool
database_term_exists (database *, const char *tname);

unsigned int
database_get_collection_freq (database *, const char *tname);

unsigned int
database_get_value_freq (database *, unsigned int valno);

const char *
database_get_value_lower_bound (database *, unsigned int valno);

const char *
database_get_value_upper_bound (database *, unsigned int valno);

unsigned int
database_get_doclength_lower_bound (database *);

unsigned int
database_get_doclength_upper_bound (database *);

unsigned int
database_get_wdf_upper_bound (database *, const char *term);

valueiterator *
database_valuestream_begin (database *, unsigned int valueno);

valueiterator *
database_valuestream_end (database *, unsigned int valueno);

unsigned int
database_get_doclength (database *, unsigned int docid);

void
database_keep_alive (database *);

document *
database_get_document (database *, unsigned int docid, const char **error);

const char * // max_edit_distance defaults to 2
database_get_spelling_suggestion (database *, const char *word,
                                  unsigned int max_edit_distance);
termiterator *
database_spellings_begin (database *);

termiterator *
database_spellings_end (database *);

termiterator *
database_synonyms_begin (database *, const char *term);

termiterator *
database_synonyms_end (database *, const char *term);

termiterator *
database_synonym_keys_begin (database *, const char *prefix);

termiterator *
database_synonym_keys_end (database *, const char *prefix);

const char *
database_get_metadata (database *, const char *key);

termiterator *
database_metadata_keys_begin (database *, const char *prefix);

termiterator *
database_metadata_keys_end (database *, const char *prefix);

const char *
database_get_uuid (database *);


/* Writable database interface */

int DB_CREATE_OR_OPEN () { return (int)Xapian::DB_CREATE_OR_OPEN; };
int DB_CREATE         () { return (int)Xapian::DB_CREATE; };
int DB_CREATE_OR_OVERWRITE () { return (int)Xapian::DB_CREATE_OR_OVERWRITE; };
int DB_OPEN           () { return (int)Xapian::DB_OPEN; };

database *
database_writable_new ();

database *
database_writable_new_from_path (const char *path, int action, const char **error);

database *
database_writable_copy (database *other);

void
database_writable_delete (database *);

void
database_commit (database *);

void
database_begin_transaction (database *, bool flushed);

void
database_commit_transaction (database *);

void
database_cancel_transaction (database *);

unsigned int
database_add_document (database *, document *doc);

void
database_delete_document_by_id (database *, unsigned int docid);

void
database_delete_document_by_term (database *, const char *unique_term);

void
database_replace_document (database *, unsigned int docid, document *doc);

void
database_add_spelling (database *, const char *word, unsigned int freqinc);

void
database_remove_spelling (database *, const char *word, unsigned int freqdec);

void
database_add_synonym (database *, const char *term, const char *synonym);

void
database_remove_synonym (database *, const char *term, const char *synonym);

void
database_clear_synonyms (database *, const char *term);

void
database_set_metadata (database *, const char *key, const char *value);

const char *
database_writable_get_description (database *);

}

#endif //CXAPIAN_DATABASE
