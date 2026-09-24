include ../Makefile.in

ifeq ($(F77COMPILER),Absoft)
  P_OBJS =  absoft_wrappers.o
else
  P_OBJS =  bit_wrappers.o trig_wrappers.o ran_wrappers.o \
    other_wrappers.o
endif

ALLOBJS = $(P_OBJS)

LIB=libport.a

#

F77=/usr/bin/gfortran

FFLAGS=-g -ffixed-line-length-132
#FFLAGS=-m32 -g -ffixed-line-length-132

INC=-I../INCLUDE/

%.o:%.f
	$(F77) $(FFLAGS) $(INC) -c $< -o $@
#


none: $(LIB)

all: $(LIB)

$(LIB): $(ALLOBJS)
	$(AR) r $@ $?

install:
	cp $(LIB) $(LIBROOT)

clean:
	$(RM) *.o $(LIB) r_*.f
