#----------------------
# GNUmakefile 
# for HKS simulator
#	by M.Kawai
#----------------------

name := HKS
#name := HKS2
G4TARGET := $(name)
G4EXLIB := true

G4SYSTEM = Linux-g++

.PHONY: all
all: lib bin

include $(G4INSTALL)/config/binmake.gmk

#CPPFLAGS += -Df2cFortran -Dextname
#LDLIBS += -lm -lg2c

RTINCLUDE = `$(ROOTSYS)/bin/root-config --cflags`
RTLIBS    = `$(ROOTSYS)/bin/root-config --libs`
CPPFLAGS  += -I$(RTINCLUDE) -g
#LDLIBS += -L$(RTLIBS) 
LDLIBS += $(RTLIBS) 
#LDLIBS += -L$(ROOTSYS)/lib -lCore 
#LDLIBS += -L$(ROOTSYS64)/lib -lCore -lMathCore -lCint -lHist -lGraf -lGraf3d -lGpad -lTree -lRint -lPostscript -lMatrix -lPhysics -lGui -pthread -lm -ldl -rdynamic -lMinuit

G4BINDIR = $(G4WORKDIR)/bin/.

cleanhks:
	rm -f src/*~ include/*~ bin/*
	rm -rf $(G4WORKDIR)/tmp/$(G4SYSTEM)/$(name)


