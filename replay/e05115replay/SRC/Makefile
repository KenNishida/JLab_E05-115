include ../../e05115src/Makefile.in

F77 = /usr/bin/g77
#FFLAGS= -m32 -ffixed-line-length-132 -finit-local-zero -I$(INC) -fbounds-check
FFLAGS= -m32 -ffixed-line-length-132 -finit-local-zero -I$(INC)
CERNLIBS = -L$(CERNLIB) -lgeant -lpawlib -lmathlib -lgraflib -lgrafX11 -lpacklib 
X11LIBS = -L/usr/X11R6/lib -lXt -lXext -lX11 -lcrypt -ldl  -lnsl -lm

ENGINELIB = $(LIBROOT)/libengine.a
UTILLIB = $(LIBROOT)/libutils.a
CTPLIB = $(LIBROOT)/libctp.a
CODALIB = $(LIBROOT)/libcoda.a
TRACKINGLIB = $(LIBROOT)/libtracking.a
ETRACKINGLIB = $(LIBROOT)/libetracking.a
HTRACKINGLIB = $(LIBROOT)/libhtracking.a
EVDISPLAYLIB = $(LIBROOT)/libevdisplay.a
PORTLIB = $(LIBROOT)/libport.a

OURLIBS = $(ENGINELIB) $(ETRACKINGLIB) $(HTRACKINGLIB) $(TRACKINGLIB) \
          $(UTILLIB) $(CTPLIB) $(CODALIB) $(EVDISPLAYLIB) $(PORTLIB)
OBJS = engine.o

.f.o: $(F77) $(FFLAGS) -c $<

none: engine_replay

all: ../EXE/engine_replay

engine: ../EXE/engine_replay

engine_replay: $(OBJS) $(OURLIBS)
	$(F77) -o ../EXE/$@ $(FFLAGS) $(OBJS) $(OURLIBS) $(CERNLIBS) $(X11LIBS)

clean:
	rm *.o ../EXE/engine_replay





