include ../../e05115src/Makefile.in

FC = /usr/bin/gfortran
F77= $(FC)
FFLAGS=  -ffixed-line-length-132 -finit-local-zero -I$(INC)

# Set for ifarm 09212026, SP
CERNLIBS = -L$(CERNLIB) -lgeant -lpawlib -lmathlib -lgraflib -lgrafX11 -lpacklib -lkernlib
X11LIBS = -L/usr/lib64 -ltirpc -lXt -lXext -lX11 -lcrypt -ldl  -lnsl -lm

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

OBJS = engine.o g_get_compileinfo.o

#COMPILEENV := '       'COMPILEINFO=\'${USER}@`/bin/hostname`:`pwd`@`/bin/date -R`\'
COMPILEENV := '       'COMPILEINFO=\'${USER}@`/bin/hostname`:rev=`svnversion -n`:`/bin/date '+%Y/%m/%d/%H:%M'`\'
none: compileenv engine_replay

all: compileenv ../EXE/engine_replay

engine: compileenv ../EXE/engine_replay

compileenv:
	echo ${COMPILEENV} > COMPILEINFO
#	/bin/rm -f g_get_compileinfo.o

%.o: %.f
	$(F77) $(FFLAGS) -c $< -o $@
#.f.o: $(F77) $(FFLAGS) -c $<

engine_replay: $(OBJS) $(OURLIBS)
	$(F77) -o ../EXE/$@ $(FFLAGS) $(OBJS) $(OURLIBS) $(CERNLIBS) $(X11LIBS)

clean:
	rm *.o ../EXE/engine_replay
