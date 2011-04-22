#include <xapian.h>
#include "cxapian_termiterator.h"


termiterator *
termiterator_new ()
{
    termiterator *self = new termiterator();
    self->iter = new Xapian::TermIterator();
    return self;
}

termiterator *
termiterator_copy (termiterator *other)
{
    termiterator *self = new termiterator();
    self->iter = new Xapian::TermIterator(*other->iter);
    return self;
}

void
termiterator_delete (termiterator *self)
{
    delete self->iter;
    delete self;
}

const char *
termiterator_get (termiterator *self)
{
    return (**self->iter).c_str();
}

void
termiterator_skip_to (termiterator *self, const char *tname)
{
    self->iter->skip_to(std::string(tname));
}

unsigned int
termiterator_get_wdf (termiterator *self)
{
    return self->iter->get_wdf();
}

unsigned int
termiterator_get_termfreq (termiterator *self)
{
    return self->iter->get_termfreq();
}

unsigned int
termiterator_positionlist_count (termiterator *self)
{
    return self->iter->positionlist_count();
}

positioniterator *
termiterator_positionlist_begin (termiterator *self)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
                        self->iter->positionlist_begin());
    return pi;
}

positioniterator *
termiterator_positionlist_end (termiterator *self)
{
    positioniterator *pi = new positioniterator();
    pi->iter = new Xapian::PositionIterator(
                        self->iter->positionlist_end());
    return pi;
}

const char *
termiterator_get_description (termiterator *self)
{
    return self->iter->get_description().c_str();
}
