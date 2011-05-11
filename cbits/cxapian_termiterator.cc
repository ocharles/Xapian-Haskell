#include <xapian.h>
#include "cxapian_termiterator.h"


Xapian::TermIterator *
termiterator_new ()
{
    return new Xapian::TermIterator();
}

Xapian::TermIterator *
termiterator_copy (Xapian::TermIterator *other)
{
    return new Xapian::TermIterator(*other);
}

void
termiterator_delete (Xapian::TermIterator *self)
{
    delete self;
}

void
termiterator_next (Xapian::TermIterator *self)
{
    (*self)++;
}

cbool
termiterator_is_end (Xapian::TermIterator *self, Xapian::TermIterator *end)
{
    return (*self == *end);
}

const char *
termiterator_get (Xapian::TermIterator *self)
{
    return (**self).c_str();
}

void
termiterator_skip_to (Xapian::TermIterator *self, const char *tname)
{
    self->skip_to(std::string(tname));
}

unsigned int
termiterator_get_wdf (Xapian::TermIterator *self)
{
    return self->get_wdf();
}

unsigned int
termiterator_get_termfreq (Xapian::TermIterator *self)
{
    return self->get_termfreq();
}

unsigned int
termiterator_positionlist_count (Xapian::TermIterator *self)
{
    return self->positionlist_count();
}

Xapian::PositionIterator *
termiterator_positionlist_begin (Xapian::TermIterator *self)
{
    return new Xapian::PositionIterator(
                        self->positionlist_begin());
}

Xapian::PositionIterator *
termiterator_positionlist_end (Xapian::TermIterator *self)
{
    return new Xapian::PositionIterator(
                        self->positionlist_end());
}

const char *
termiterator_get_description (Xapian::TermIterator *self)
{
    return self->get_description().c_str();
}
