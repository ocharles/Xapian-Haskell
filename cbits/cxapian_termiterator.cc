#include <xapian.h>
#include "cxapian_termiterator.h"


termiterator *
termiterator_new ()
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator();
    return ti;
}

termiterator *
termiterator_copy (termiterator *original)
{
    termiterator *ti = new termiterator();
    ti->iter = new Xapian::TermIterator(*original->iter);
    return ti;
}

const char *
termiterator_get (termiterator *ti)
{
    return (**ti->iter).c_str();
}

void
termiterator_skip_to (termiterator *ti, const char *tname)
{
    ti->iter->skip_to(std::string(tname));
}

unsigned int
termiterator_get_wdf (termiterator *ti)
{
    return ti->iter->get_wdf();
}

unsigned int
termiterator_get_termfreq (termiterator *ti)
{
    return ti->iter->get_termfreq();
}

unsigned int
termiterator_positionlist_count (termiterator *ti)
{
    return ti->iter->positionlist_count();
}

positioniterator *
termiterator_positionlist_begin (termiterator *ti)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
                        ti->iter->positionlist_begin());
    return pi;
}

positioniterator *
termiterator_positionlist_end (termiterator *ti)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
                        ti->iter->positionlist_end());
    return pi;
}

const char *
termiterator_get_description (termiterator *ti)
{
    return ti->iter->get_description().c_str();
}
