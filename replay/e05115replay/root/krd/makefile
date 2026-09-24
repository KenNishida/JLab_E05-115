#Makefile

ROOTLIBS = `$(ROOTSYS64)/bin/root-config --libs`
ROOTINCLUDE = `$(ROOTSYS64)/bin/root-config --cflags`
#ROOTLIBS = `root-config --libs`
#ROOTINCLUDE = `root-config --cflags`
#LDLIBS = $(ROOTLIBS) $(ROOTGLIBS) -lm -lCore -lCint -lHist -lGraf -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix -lPhysics -lGui -pthread -lm -ldl -rdynamic -lMinuit
LDLIBS = $(ROOTLIBS)
#LDLIBS += -L$(ROOTSYS64)/lib -lCore -lMathCore -lCint -lHist -lGraf -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix -lPhysics -lGui -lm -ldl -rdynamic -lMinuit
CC = g++
#CCFLAGS = -O  -D__osf__ -D__alpha -I$(ROOTINCLUDE)
#CCFLAGS = -I$(ROOTINCLUDE) -I./include/
CCFLAGS = $(ROOTINCLUDE) -I./include/
TARGET1 = krd
OBJS1 =  main.o krd.o 
TARGET2 = merge 
OBJS2 =  merge.o 

all: ${TARGET1} ${TARGET2}

${TARGET1}: ${OBJS1}
	${CC} ${CCFLAGS} -o $@ ${OBJS1} ${LDLIBS}

main.o: main.C include/coin.h 
	${CC} ${CCFLAGS} -c  main.C ${LDLIBS}

krd.o: krd.C include/coin.h include/krd.h include/constant.h
	${CC} ${CCFLAGS} -c  krd.C ${LDLIBS}

${TARGET2}: ${OBJS2}
	${CC} ${CCFLAGS} -o $@ ${OBJS2} ${LDLIBS}

merge.o: merge.C
	${CC} ${CCFLAGS} -c  merge.C ${LDLIBS}

#.cc.o:
#	${CC} ${CCFLAGS} -c $<

clean:
	rm -rf *.o core ${TARGET1}

