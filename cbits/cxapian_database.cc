#include <xapian.h>
#include "cxapian_database.h"

Xapian::Database *
database_new ()
{
    return new Xapian::Database();
}

Xapian::Database *
database_new_from_path (const char *path, const char **error)
{
    Xapian::Database *db;
    try {
        db = new Xapian::Database(std::string(path));
        return db;
    }
    catch (const Xapian::Error &e) {
        *error = e.get_msg().c_str();
        delete db;
        return NULL;
    }
}

Xapian::Database *
database_copy (Xapian::Database *other)
{
    return new Xapian::Database(*other);
}

void
database_delete (Xapian::Database *db)
{
    delete db;
}

void
database_add_database (Xapian::Database *db, Xapian::Database *other)
{
    db->add_database(*other);
}

void
database_reopen (Xapian::Database *db)
{
    db->reopen();
}

void
database_close (Xapian::Database *db)
{
    db->close();
}

const char *
database_get_description (Xapian::Database *db)
{
    return db->get_description().c_str();;
}

Xapian::PostingIterator *
database_postlist_begin (Xapian::Database *db, std::string *term)
{
    return new Xapian::PostingIterator( db->postlist_begin(*term) );
}

Xapian::PostingIterator *
database_postlist_end (Xapian::Database *db, std::string *term)
{
    return new Xapian::PostingIterator( db->postlist_end(*term) );
}

Xapian::TermIterator *
database_termlist_begin (Xapian::Database *db, unsigned int docid)
{
    return new Xapian::TermIterator(
            db->termlist_begin(docid) );
}

Xapian::TermIterator *
database_termlist_end (Xapian::Database *db, unsigned int docid)
{
    return new Xapian::TermIterator(
            db->termlist_end(docid) );
}

cbool
database_has_positions (Xapian::Database *db)
{
    return (cbool)db->has_positions();
}

Xapian::PositionIterator *
database_positionlist_begin (Xapian::Database *db, unsigned int docid, std::string *term)
{
    return new Xapian::PositionIterator( db->positionlist_begin(docid, *term) );
}

Xapian::PositionIterator *
database_positionlist_end (Xapian::Database *db, unsigned int docid, std::string *term)
{
    return new Xapian::PositionIterator( db->positionlist_end(docid, *term) );
}

Xapian::TermIterator *
database_allterms_begin (Xapian::Database *db)
{
    return new Xapian::TermIterator( db->allterms_begin() );
}

Xapian::TermIterator *
database_allterms_end (Xapian::Database *db)
{
    return new Xapian::TermIterator( db->allterms_end() );
}

Xapian::TermIterator *
database_allterms_with_prefix_begin (Xapian::Database *db, std::string *prefix)
{
    return new Xapian::TermIterator( db->allterms_begin(*prefix) );
}

Xapian::TermIterator *
database_allterms_with_prefix_end (Xapian::Database *db, std::string *prefix)
{
    return new Xapian::TermIterator( db->allterms_end( *prefix) );
}

unsigned int
database_get_doccount (Xapian::Database *db)
{
    return db->get_doccount();
}

unsigned int
database_get_lastdocid (Xapian::Database *db)
{
    return db->get_lastdocid();
}

double
database_get_avlength (Xapian::Database *db)
{
    return db->get_avlength();
}

unsigned int
database_get_termfreq (Xapian::Database *db, std::string *term)
{
    return db->get_termfreq( *term );
}

cbool
database_term_exists (Xapian::Database *db, std::string *term)
{
    return db->term_exists( *term );
}

unsigned int
database_get_collection_freq (Xapian::Database *db, std::string *term)
{
    return db->get_collection_freq( *term );
}

unsigned int
database_get_value_freq (Xapian::Database *db, unsigned int valno)
{
    return db->get_value_freq( valno );
}

std::string *
database_get_value_lower_bound (Xapian::Database *db, unsigned int valno)
{
    std::string *str = new std::string(db->get_value_lower_bound( valno ));
    return str;
}

std::string *
database_get_value_upper_bound (Xapian::Database *db, unsigned int valno)
{
    std::string *str = new std::string(db->get_value_upper_bound( valno ));
    return str;
}

unsigned int
database_get_doclength_lower_bound (Xapian::Database *db)
{
    return db->get_doclength_lower_bound();
}

unsigned int
database_get_doclength_upper_bound (Xapian::Database *db)
{
    return db->get_doclength_upper_bound();
}

unsigned int
database_get_wdf_upper_bound (Xapian::Database *db, std::string *term)
{
    return db->get_wdf_upper_bound( *term );
}

Xapian::ValueIterator *
database_valuestream_begin (Xapian::Database *db, unsigned int valueno)
{
    return new Xapian::ValueIterator( db->valuestream_begin(valueno) );
}

Xapian::ValueIterator *
database_valuestream_end (Xapian::Database *db, unsigned int valueno)
{
    return new Xapian::ValueIterator( db->valuestream_end(valueno) );
}

unsigned int
database_get_doclength (Xapian::Database *db, unsigned int docid)
{
    return db->get_doclength(docid);
}

void
database_keep_alive (Xapian::Database *db)
{
    return db->keep_alive();
}

Xapian::Document *
database_get_document (Xapian::Database *db, unsigned int docid, const char **error)
{
    Xapian::Document *doc = NULL; 
    try {
        doc = new Xapian::Document( db->get_document(docid) );
        return doc;
    }
    catch (const Xapian::Error &e) {
        *error = e.get_msg().c_str();
        if (doc) delete doc;
        return NULL;
    }
}

std::string * // max_edit_distance defaults to 2
database_get_spelling_suggestion (Xapian::Database *db, std::string *word,
                                  unsigned int max_edit_distance)
{
    std::string *str = new std::string(
            db->get_spelling_suggestion( *word, max_edit_distance ) );
    return str;
}

Xapian::TermIterator *
database_spellings_begin (Xapian::Database *db)
{
    return new Xapian::TermIterator( db->spellings_begin() );
}

Xapian::TermIterator *
database_spellings_end (Xapian::Database *db)
{
    return new Xapian::TermIterator( db->spellings_end() );
}

Xapian::TermIterator *
database_synonyms_begin (Xapian::Database *db, std::string *term)
{
    return new Xapian::TermIterator( db->synonyms_begin(*term) );
}

Xapian::TermIterator *
database_synonyms_end (Xapian::Database *db, std::string *term)
{
    return new Xapian::TermIterator( db->synonyms_end(*term) );
}

Xapian::TermIterator *
database_synonym_keys_begin (Xapian::Database *db, std::string *prefix)
{
    return new Xapian::TermIterator( db->synonym_keys_begin(*prefix) );
}


Xapian::TermIterator *
database_synonym_keys_end (Xapian::Database *db, std::string *prefix)
{
    return new Xapian::TermIterator( db->synonym_keys_end(*prefix) );
}

std::string *
database_get_metadata (Xapian::Database *db, std::string *key)
{
    std::string *str = new std::string(db->get_metadata(*key));
    return str;
}

Xapian::TermIterator *
database_metadata_keys_begin (Xapian::Database *db, std::string *prefix)
{
    return new Xapian::TermIterator( db->metadata_keys_begin(*prefix) );
}

Xapian::TermIterator *
database_metadata_keys_end (Xapian::Database *db, std::string *prefix)
{
    return new Xapian::TermIterator( db->metadata_keys_end(*prefix) );
}

std::string *
database_get_uuid (Xapian::Database *db)
{
    std::string *str = new std::string (db->get_uuid());
    return str;
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


Xapian::WritableDatabase *
database_writable_new ()
{
    return new Xapian::WritableDatabase();
}

Xapian::WritableDatabase *
database_writable_new_from_path (const char *path, int action, const char **error)
{
    Xapian::WritableDatabase *db;
    try {
        db = new Xapian::WritableDatabase( std::string(path), action );
        return db;
    }
    catch (const Xapian::Error & e) {
        *error = e.get_msg().c_str();
        delete db;
        return NULL;
    }
}

Xapian::WritableDatabase *
database_writable_copy (Xapian::WritableDatabase *other)
{
    return new Xapian::WritableDatabase(*other);
}

void
database_writable_delete (Xapian::WritableDatabase *db)
{
    delete db;
}

void
database_commit (Xapian::WritableDatabase *db)
{
    db->commit();
}

void
database_begin_transaction (Xapian::WritableDatabase *db, cbool flushed)
{
    db->begin_transaction();
}

void
database_commit_transaction (Xapian::WritableDatabase *db)
{
    db->commit_transaction();
}

void
database_cancel_transaction (Xapian::WritableDatabase *db)
{
    db->cancel_transaction();
}

unsigned int
database_add_document (Xapian::WritableDatabase *db, Xapian::Document *doc)
{
    return db->add_document(*doc);
}

void
database_delete_document_by_id (Xapian::WritableDatabase *db, unsigned int docid)
{
    db->delete_document(docid);
}

void
database_delete_document_by_term (Xapian::WritableDatabase *db, std::string *unique_term)
{
    db->delete_document( *unique_term );
}

void
database_replace_document (Xapian::WritableDatabase *db, unsigned int docid, Xapian::Document *doc)
{
    db->replace_document(docid, *doc);
}

void
database_add_spelling (Xapian::WritableDatabase *db, std::string *word, unsigned int freqinc)
{
    db->add_spelling(*word, freqinc);
}

void
database_remove_spelling (Xapian::WritableDatabase *db, std::string *word, unsigned int freqdec)
{
    db->remove_spelling(*word, freqdec);
}

void
database_add_synonym (Xapian::WritableDatabase *db, std::string *term, std::string *synonym)
{
    db->add_synonym(*term, *synonym);
}

void
database_remove_synonym (Xapian::WritableDatabase *db, std::string *term, std::string *synonym)
{
    db->remove_synonym(*term, *synonym);
}

void
database_clear_synonyms (Xapian::WritableDatabase *db, std::string *term)
{
    db->clear_synonyms(*term);
}

void
database_set_metadata (Xapian::WritableDatabase *db, std::string *key, std::string *value)
{
    db->set_metadata(*key, *value);
}

const char *
database_writable_get_description (Xapian::WritableDatabase *db)
{
    return db->get_description().c_str();
}
