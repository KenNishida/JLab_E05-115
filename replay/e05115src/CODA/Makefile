include ../Makefile.in

ALLOBJS= ceMsgLib.o ceMsgTbl.o evio.o swap_util.o

LIB= libcoda.a
GCC=/usr/bin/gcc

.c.o:
	$(GCC) -c $< -o $@
#	$(GCC) -m32 -c $< -o $@

none: $(LIB)

all: $(LIB)

$(LIB): $(ALLOBJS)
	$(AR) r $@ $?

install:
	cp $(LIB) $(LIBROOT)

clean:
	$(RM) *.o $(LIB)
