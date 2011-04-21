#include <xapian.h>
#include "cxapian_stem.h"

stem *
stem_copy (stem *original)
{
    stem *s = new stem();
    s->get = new Xapian::Stem(*original->get);
    return s;
}

stem *
stem_new_with_language (const char *lang)
{
    stem *s = new stem();
    s->get = new Xapian::Stem(std::string(lang));
    return s;
}

//stem_new_with_stemimplementation

void
stem_delete (stem *s)
{
    delete s->get;
    delete s;
}

const char *
stem_word (stem *s, const char *word)
{
    return (*s->get)(std::string(word)).c_str();
}

const char *
stem_get_description (stem *s)
{
    return s->get->get_description().c_str();
}

const char *
stem_get_available_languages ()
{
    return Xapian::Stem::get_available_languages().c_str();
}
