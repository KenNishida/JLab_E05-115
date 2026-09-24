include ../Makefile.in

ALLOBJS= find_space_points.o select_space_points.o select_space_points_seva.o solve_four_by_four.o select_space_points_hks.o

LIB= libtracking.a

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
	$(RM) *.o $(LIB)
