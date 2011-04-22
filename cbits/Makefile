CPP = /usr/bin/g++
CPPFLAGS = -Wall
LDFLAGS = -lxapian -shared

OBJ = cxapian_database.o cxapian_document.o cxapian_enquire.o cxapian_mset.o cxapian_msetiterator.o cxapian_positioniterator.o cxapian_postingiterator.o cxapian_query.o cxapian_stem.o cxapian_stopper.o cxapian_termgenerator.o cxapian_termiterator.o cxapian_valueiterator.o

libcxapian.so: $(OBJ)
	$(CPP) $(CPPFLAGS) -o libcxapian.so $(OBJ) $(LDFLAGS)
	
%.o: %.cc
	$(CPP) $(CPPFLAGS) -c $<
