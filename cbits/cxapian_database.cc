#include <xapian.h>
#include "cxapian_database.h"

database *
database_new ()
{
    database *db = new database();
    db->get = new Xapian::Database();
    return db;
}

database *
database_new_from_path (const char *path, const char **error)
{
    database *db = new database();
    try {
        db->get = new Xapian::Database(std::string(path));
        return db;
    }
    catch (const Xapian::Error &e) {
        *error = e.get_msg().c_str();
        delete db;
        return NULL;
    }
}

database *
database_copy (database *other)
{
    database *db = new database();
    db->get = new Xapian::Database(*other->get);
    return db;
}

void
database_delete (database *db)
{
    delete db->get;
    delete db;
}

void
database_add_database (database *db, database *other)
{
    db->get->add_database(*other->get);
}

void
database_reopen (database *db)
{
    db->get->reopen();
}

void
database_close (database *db)
{
    db->get->close();
}

const char *
database_get_description (database *db)
{
    return db->get->get_description().c_str();;
}

postingiterator *
database_postlist_begin (database *db, const char *tname)
{
    postingiterator *pi = new postingiterator();
    pi->iter = new Xapian::PostingIterator(
            db->get->postlist_begin(std::string(tname)) );
    return pi;
}

postingiterator *
database_postlist_end (database *db, const char *tname)
{
    postingiterator *pi = new postingiterator();
    pi->iter = new Xapian::PostingIterator(
            db->get->postlist_end(std::string(tname)) );
    return pi;
}

termiterator *
database_termlist_begin (database *db, unsigned int docid)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->termlist_begin(docid) );
    return ti;
}

termiterator *
database_termlist_end (database *db, unsigned int docid)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->termlist_end(docid) );
    return ti;
}

bool
database_has_positions (database *db)
{
    return db->get->has_positions();
}

positioniterator *
database_positionlist_begin (database *db, unsigned int docid, const char *tname)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
            db->get->positionlist_begin(docid, std::string(tname)) );
    return pi;
}

positioniterator *
database_positionlist_end (database *db, unsigned int docid, const char *tname)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
            db->get->positionlist_end(docid, std::string(tname)) );
    return pi;
}

termiterator *
database_allterms_begin (database *db)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator( db->get->allterms_begin() );
    return ti;
}

termiterator *
database_allterms_end (database *db)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator( db->get->allterms_end() );
    return ti;
}

termiterator *
database_allterms_with_prefix_begin (database *db, const char *prefix)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->allterms_begin(std::string(prefix)) );
    return ti;
}

termiterator *
database_allterms_with_prefix_end (database *db, const char *prefix)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->allterms_end(std::string(prefix)) );
    return ti;
}

unsigned int
database_get_doccount (database *db)
{
    return db->get->get_doccount();
}

unsigned int
database_get_lastdocid (database *db)
{
    return db->get->get_lastdocid();
}

double
database_get_avlength (database *db)
{
    return db->get->get_avlength();
}

unsigned int
database_get_termfreq (database *db, const char *tname)
{
    return db->get->get_termfreq( std::string(tname) );
}

bool
database_term_exists (database *db, const char *tname)
{
    return db->get->term_exists( std::string(tname) );
}

unsigned int
database_get_collection_freq (database *db, const char *tname)
{
    return db->get->get_collection_freq( std::string(tname) );
}

unsigned int
database_get_value_freq (database *db, unsigned int valno)
{
    return db->get->get_value_freq( valno );
}

const char *
database_get_value_lower_bound (database *db, unsigned int valno)
{
    return db->get->get_value_lower_bound( valno ).c_str();
}

const char *
database_get_value_upper_bound (database *db, unsigned int valno)
{
    return db->get->get_value_upper_bound( valno ).c_str();
}

unsigned int
database_get_doclength_lower_bound (database *db)
{
    return db->get->get_doclength_lower_bound();
}

unsigned int
database_get_doclength_upper_bound (database *db)
{
    return db->get->get_doclength_upper_bound();
}

unsigned int
database_get_wdf_upper_bound (database *db, const char *term)
{
    return db->get->get_wdf_upper_bound( std::string(term) );
}

valueiterator *
database_valuestream_begin (database *db, unsigned int valueno)
{
    valueiterator *vi = new valueiterator();
    vi->iter = new Xapian::ValueIterator( db->get->valuestream_begin(valueno) );
    return vi;
}

valueiterator *
database_valuestream_end (database *db, unsigned int valueno)
{
    valueiterator *vi = new valueiterator();
    vi->iter = new Xapian::ValueIterator( db->get->valuestream_end(valueno) );
    return vi;
}

unsigned int
database_get_doclength (database *db, unsigned int docid)
{
    return db->get->get_doclength(docid);
}

void
database_keep_alive (database *db)
{
    return db->get->keep_alive();
}

document *
database_get_document (database *db, unsigned int docid, const char **error)
{
    document *doc = new document();
    try {
        doc->get = new Xapian::Document( db->get->get_document(docid) );
        return doc;
    }
    catch (const Xapian::Error &e) {
        *error = e.get_msg().c_str();
        delete doc;
        return NULL;
    }
}

const char * // max_edit_distance defaults to 2
database_get_spelling_suggestion (database *db, const char *word,
                                  unsigned int max_edit_distance)
{
    return db->get->get_spelling_suggestion(
            std::string(word), max_edit_distance ).c_str();
}
termiterator *
database_spellings_begin (database *db)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator( db->get->spellings_begin() );
    return ti;
}

termiterator *
database_spellings_end (database *db)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator( db->get->spellings_end() );
    return ti;
}

termiterator *
database_synonyms_begin (database *db, const char *term)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->synonyms_begin(std::string(term)) );
    return ti;
}

termiterator *
database_synonyms_end (database *db, const char *term)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->synonyms_end(std::string(term)) );
    return ti;
}

termiterator *
database_synonym_keys_begin (database *db, const char *prefix)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->synonym_keys_begin(std::string(prefix)) );
    return ti;
}


termiterator *
database_synonym_keys_end (database *db, const char *prefix)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->synonym_keys_end(std::string(prefix)) );
    return ti;
}

const char *
database_get_metadata (database *db, const char *key)
{
    return db->get->get_metadata(std::string(key)).c_str();
}

termiterator *
database_metadata_keys_begin (database *db, const char *prefix)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->metadata_keys_begin(std::string(prefix)) );
    return ti;
}

termiterator *
database_metadata_keys_end (database *db, const char *prefix)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(
            db->get->metadata_keys_end(std::string(prefix)) );
    return ti;
}

const char *
database_get_uuid (database *db)
{
    return db->get->get_uuid().c_str();
}


/* Writable database interface */

/* include from 'xapian/database.h':
 *
 * DB_CREATE_OR_OPEN
 * DB_CREATE
 * DB_CREATE_OR_OVERWRITE
 * DB_OPEN
 *
 */

static Xapian::WritableDatabase *
__writable_db (Xapian::Database *db)
{
    return (Xapian::WritableDatabase *) db;
}

database *
database_writable_new ()
{
    database *db = new database();
    db->get = (Xapian::Database *) new Xapian::WritableDatabase();
    return db;
}

database *
database_writable_new_from_path (const char *path, int action, const char **error)
{
    database *db = new database();
    try {
        db->get = (Xapian::Database *) new Xapian::WritableDatabase(
                std::string(path), action );
        return db;
    }
    catch (const Xapian::Error & e) {
        *error = e.get_msg().c_str();
        delete db;
        return NULL;
    }
}

database *
database_writable_copy (database *other)
{
    database *db = new database();
    db->get = (Xapian::Database *) new Xapian::WritableDatabase(
            *(__writable_db(other->get)));
    return db;
}

void
database_writable_delete (database *db)
{
    database_commit(db);
    delete (__writable_db(db->get));
    delete db;
}

void
database_commit (database *db)
{
    (__writable_db(db->get))->commit();
}

void
database_begin_transaction (database *db, bool flushed)
{
    (__writable_db(db->get))->begin_transaction();
}

void
database_commit_transaction (database *db)
{
    (__writable_db(db->get))->commit_transaction();
}

void
database_cancel_transaction (database *db)
{
    (__writable_db(db->get))->cancel_transaction();
}

unsigned int
database_add_document (database *db, document *doc)
{
    return (__writable_db(db->get))->add_document(*doc->get);
}

void
database_delete_document_by_id (database *db, unsigned int docid)
{
    (__writable_db(db->get))->delete_document(docid);
}

void
database_delete_document_by_term (database *db, const char *unique_term)
{
    (__writable_db(db->get))->delete_document(std::string(unique_term));
}

void
database_replace_document (database *db, unsigned int docid, document *doc)
{
    (__writable_db(db->get))->replace_document(docid, *doc->get);
}

void
database_add_spelling (database *db, const char *word, unsigned int freqinc)
{
    (__writable_db(db->get))->add_spelling(std::string(word), freqinc);
}

void
database_remove_spelling (database *db, const char *word, unsigned int freqdec)
{
    (__writable_db(db->get))->remove_spelling(std::string(word), freqdec);
}

void
database_add_synonym (database *db, const char *term, const char *synonym)
{
    (__writable_db(db->get))->add_synonym(std::string(term), std::string(synonym));
}

void
database_remove_synonym (database *db, const char *term, const char *synonym)
{
    (__writable_db(db->get))->remove_synonym(std::string(term), std::string(synonym));
}

void
database_clear_synonyms (database *db, const char *term)
{
    (__writable_db(db->get))->clear_synonyms(std::string(term));
}

void
database_set_metadata (database *db, const char *key, const char *value)
{
    (__writable_db(db->get))->set_metadata(std::string(key), std::string(value));
}

const char *
database_writable_get_description (database *db)
{
    return (__writable_db(db->get))->get_description().c_str();
}
