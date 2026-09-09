#Makefile

ROOTLIBS = `root-config --libs`
ROOTINCLUDE = `root-config --cflags`
GLIBS = $(ROOTLIBS) $(ROOTGLIBS) -lm -lCore -lCint -lHist -lGraf -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix -lPhysics -lGui -pthread -lm -ldl -rdynamic -lMinuit
CC = g++
#CCFLAGS = -O  -D__osf__ -D__alpha -I$(ROOTINCLUDE)
CCFLAGS = -I$(ROOTINCLUDE) -I./include/
LD = g++
LDFLAGS = -g
TARGET = krd
OBJS =  main.o krd.o 

all: ${TARGET}

${TARGET}: ${OBJS}
	${CC} ${CCFLAGS} -o $@ ${OBJS} ${GLIBS}

main.o: main.C include/coin.h 
	${CC} ${CCFLAGS} -c  main.C ${GLIBS}

krd.o: krd.C include/coin.h include/krd.h include/constant.h
	${CC} ${CCFLAGS} -c  krd.C ${GLIBS}

#.cc.o:
#	${CC} ${CCFLAGS} -c $<

clean:
	rm -rf *.o core ${TARGET}

