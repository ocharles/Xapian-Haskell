#include <xapian.h>
#include "cxapian_stem.h"

Xapian::Stem *
stem_copy (Xapian::Stem *original)
{
    return new Xapian::Stem(*original);
}

Xapian::Stem *
stem_new_with_language (const char *lang)
{
    return new Xapian::Stem(std::string(lang));
}

//stem_new_with_stemimplementation

void
stem_delete (Xapian::Stem *s)
{
    delete s;
}

const char *
stem_word (Xapian::Stem *s, const char *word)
{
    return (*s)(std::string(word)).c_str();
}

const char *
stem_get_description (Xapian::Stem *s)
{
    return s->get_description().c_str();
}

const char *
stem_get_available_languages ()
{
    return Xapian::Stem::get_available_languages().c_str();
}
