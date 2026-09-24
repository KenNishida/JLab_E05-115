include ../Makefile.in

OBJS= evdisplay_init.o \
      evdisplay_end.o \
      both_detector_drawing.o \
      hks_detector_drawing.o \
      hks_hits_drawing.o \
      hks_has_valid_hits.o \
      hes_detector_drawing.o \
      hes_hits_drawing.o \
      hes_has_valid_hits.o

ALLOBJS= $(OBJS)

LIB= libevdisplay.a

F77=/usr/bin/gfortran

FFLAGS=-g -ffixed-line-length-132
#FFLAGS=-m32 -g -ffixed-line-length-132

INC=-I../INCLUDE/ -I./

%.o:%.f
	$(F77) $(FFLAGS) $(INC) -c $< -o $@


none: $(LIB)

all: $(LIB)

$(LIB): $(ALLOBJS)
	$(AR) r $@ $?

install:
	cp $(LIB) $(LIBROOT)

clean:
	$(RM) *.o $(LIB) r_*.f
