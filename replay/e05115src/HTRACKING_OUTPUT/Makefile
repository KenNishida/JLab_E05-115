include ../Makefile.in
ROOT_CONFIG ?= root-config
CXX ?= g++
ROOTCFLAGS := $(shell $(ROOT_CONFIG) --cflags)
CXXFLAGS += -g -O2 -std=c++17 $(ROOTCFLAGS)
LIB=libhtrackingoutput.a
all: $(LIB)
none: all
htracking_writer.o: htracking_writer.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@
$(LIB): htracking_writer.o
	$(AR) rcs $@ $<
install: $(LIB)
	cp $(LIB) $(LIBROOT)
clean:
	$(RM) *.o $(LIB)
